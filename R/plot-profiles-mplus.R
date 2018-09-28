#' Plot variable means and their confidence intervals by profile for models estimated with MPlus (requires purchasing and installing MPlus to use)
#' @details Plot the variable means and variances for data frame output from estimate_profiles_mclust()
#' @param mplus_data output from estimate_profiles_mplus() with return_savedata = T specified
#' @param plot_post_probs logical; whether to plot the means and standard errors based on the posterior probabilities (if TRUE) or on the classification/profile (if FALSE); defaults to TRUE
#' @param mplus_out_name character string; name of the mplus out file that is read if the posterior probabilities are plotted; defaults to i.out (which is the default name for the file created by other tidyLPA functions)
#' @param standard_error_interval number between 0 and 1; defaults to .95
#' @inheritParams plot_profiles
#' @export

plot_profiles_mplus <- function(mplus_data, to_center = TRUE, to_scale = TRUE, plot_post_probs = FALSE, mplus_out_name = "i.out", standard_error_interval = .95) {

    if (plot_post_probs == TRUE) {

        x <- suppressWarnings(MplusAutomation::readModels(mplus_out_name))
        number_of_rows <- nrow(x$savedata)
        se_diff <- standard_error_interval + ((1 - standard_error_interval) / 2)
        z <- count(x$savedata, .data$C)
        profile_names <- paste0("Profile ", z$C, " (n = ", z$n, ")")

        p <- seq(x$tech7) %>%
            map_df(extract_prob_stats, obj = x$tech7) %>%
            ggplot(aes(x = as.character(class),
                       y = .data$class_mean,
                       ymin = .data$class_mean - (1.96 * ((.data$class_sd / sqrt(number_of_rows)))),
                       ymax = .data$class_mean + (1.96 * ((.data$class_sd / sqrt(number_of_rows)))),
                       fill = .data$var
                       )) +
            geom_col(position = "dodge") +
            geom_errorbar(position = "dodge") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_x_discrete("", labels = profile_names) +
            ylab("Mean")

        return(p)

    }

    mplus_data <- mplus_data[, -ncol(mplus_data)]

    z <- count(mplus_data, .data$C)

    d <- mplus_data %>%
        left_join(z, by = "C") %>%
        mutate(profile = paste0("Profile ", .data$C, " (n = ", .data$n, ")")) %>%
        select(-contains("CPROB"), -.data$C, -.data$n) %>%
        mutate_at(vars(-.data$profile),
                  scale,
                  center = to_center,
                  scale = to_scale
        ) %>%
        dplyr::slice(1:nrow(.)) %>% # this is to overcome a weird bug in the subsequent gather function, which outputs a warning
        group_by(.data$profile) %>%
        summarize_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE))) %>%
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
