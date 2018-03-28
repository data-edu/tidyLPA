#' Plot variable means and variances by profile for MPlus output (requires purchasing and installing MPlus to use)
#' @details Plot the variable means and variances for data frame output from estimate_profiles_mclust()
#' @param mplus_data output from estimate_profiles_mclust() with return_savedata = T specified
#' @inheritParams plot_profiles
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export

plot_profiles_mplus <- function(mplus_data, to_center = T, to_scale = T) {
  message("Note that this (and other functions that use MPlus) is at the experimental stage! Please provide feedback at https://github.com/jrosen48/tidyLPA")

  z <- count(mplus_data, .data$C)

  d <- mplus_data %>%
    left_join(z, by = "C") %>%
    mutate(profile = paste0("Profile ", .data$C, " (n = ", .data$n, ")")) %>%
    select(-contains("CPROB"), -.data$C, -.data$n) %>%
    mutate_at(vars(-.data$profile), scale, center = to_center, scale = to_scale) %>%
    group_by(.data$profile) %>%
    summarize_all(funs(mean, sd)) %>%
    gather("key", "val", -.data$profile) %>%
    mutate(
      new_key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, start = -4),
        ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, start = -2), NA)
      ),
      key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, end = -6),
        ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, end = -4), NA)
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

  p <- ggplot(d, aes_string(x = "profile", y = "mean", fill = "key", ymin = "ymin", ymax = "ymax")) +
    geom_col(position = "dodge") +
    geom_errorbar(position = position_dodge()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete("")

  p
}
