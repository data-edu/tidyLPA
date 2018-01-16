#' Plot profile centroids for mplus output
#' @details Plot the centroids for tibble or mclust output from create_profiles_mplus()
#' @param mplus_data output from create_profiles_mclust() with return_savedata = T specified
#' @inheritParams plot_profiles_lpa
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export

plot_profiles_mplus <- function(mplus_data, to_center = T, to_scale = T) {

    if (is.data.frame(mplus_data)[[2]]) {
        stop("Did you specify return_savedata = T in create_profiles_mplus()? If not, add that argument to create_profiles_mplus() and run plot_profiles_mplus() again.")
    }

    z <- mplus_data[[2]] %>% count(C)

    p <- mplus_data[[2]] %>%
        left_join(z) %>%
        mutate(profile = paste0("Profile ", C, " (n = ", n, ")")) %>%
        select(-contains("CPROB"), -C, -n) %>%
        mutate_at(vars(-.data$profile), scale, center = to_center, scale = to_scale) %>%
        group_by(profile) %>%
        summarize_all(funs(mean, sd)) %>%
        gather(key, val, -profile) %>%
        mutate(new_key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, start = -4),
                                ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, start = -2), NA)),
               key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, end = -6),
                            ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, end = -4), NA))) %>%
        spread(.data$new_key, .data$val) %>%
        mutate(n_string = str_sub(as.character(.data$profile), start = 11),
               n = as.numeric(str_extract(.data$n_string, "\\-*\\d+\\.*\\d*")),
               se = 1.96 * (.data$sd / sqrt(.data$n - 1)),
               ymin = .data$mean - .data$se,
               ymax = .data$mean + .data$se) %>%
        ggplot(aes_string(x = "profile", y = "mean", fill = "key", ymin = "ymin", ymax = "ymax")) +
        geom_col(position = "dodge") +
        geom_errorbar(position = position_dodge()) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete("")

    p

}
