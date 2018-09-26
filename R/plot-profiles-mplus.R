#' Plot variable means and their confidence intervals by profile for models estimated with MPlus (requires purchasing and installing MPlus to use)
#' @details Plot the variable means and variances for data frame output from estimate_profiles_mclust()
#' @param mplus_data output from estimate_profiles_mplus() with return_savedata = T specified
#' @param weight_data logical; whether to plot the means and standard errors weighted by the posterior probabilities; defaults to TRUE
#' @inheritParams plot_profiles
#' @export

plot_profiles_mplus <- function(mplus_data,
                                to_center = TRUE,
                                to_scale = TRUE,
                                weight_data = TRUE) {

  # mplus_data <- mplus_data[, -ncol(mplus_data)]

  z <- count(mplus_data, .data$C)

  max_prob <- select(mplus_data, contains("CPROB", ignore.case = FALSE)) %>%
      mutate(ID = 1:nrow(.)) %>%
      gather(key, val, -ID) %>%
      group_by(ID) %>%
      summarize(max_prob = max(val))

  d <- mplus_data %>%
    left_join(z, by = "C") %>%
    left_join(max_prob, by = "ID") %>%
    select(-ID) %>%
    mutate(profile = paste0("Profile ", .data$C, " (n = ", .data$n, ")")) %>%
    select(-contains("CPROB"), -C, -n) %>%
    mutate_at(vars(-.data$profile, -max_prob),
      scale,
      center = to_center,
      scale = to_scale
    ) %>%
    slice(1:nrow(.)) %>% # this is to overcome a weird bug in the subsequent gather function, which outputs a warning
    group_by(profile)
    # summarize_at(vars(-m, -C), funs(mean), w = m)
    summarize_at(vars(-max_prob, -profile), funs(weighted.mean), w = max_prob)
    gather("key", "val", -.data$profile) %>%
    mutate(
      new_key = ifelse(str_sub(.data$key, start = -4) == "mean",
        str_sub(.data$key, start = -4),
        ifelse(str_sub(.data$key, start = -2) == "sd",
          str_sub(.data$key, start = -2),
          NA
        )
      ),
      key = ifelse(str_sub(.data$key, start = -4) == "mean",
        str_sub(.data$key, end = -6),
        ifelse(str_sub(.data$key, start = -2) == "sd",
          str_sub(.data$key, end = -4),
          NA
        )
      )
    ) %>%
    spread(.data$new_key, .data$val) %>%
    mutate(
      n_string = str_sub(as.character(.data$profile), start = 11),
      n = as.numeric(str_extract(.data$n_string, "\\-*\\d+\\.*\\d*")),
      se = 1.96 * (.data$sd / sqrt(.data$n - 1)),
      ymin = .data$mean - .data$se,
      ymax = .data$mean + .data$se
    )

  p <- ggplot(d, aes_string(
    x = "profile",
    y = "mean",
    fill = "key",
    ymin = "ymin",
    ymax = "ymax"
  )) +
    geom_col(position = "dodge") +
    geom_errorbar(position = position_dodge()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete("")

  p
}
