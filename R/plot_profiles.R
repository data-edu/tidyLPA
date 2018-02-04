#' Plot profile centroids
#' @details Plot the centroids for tibble or mclust output from estimate_profiles()
#' @param x output from create_profiles_mclust()
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @param plot_what whether to plot tibble or mclust output from estimate_profiles(); defaults to tibble
#' @param plot_error_bars whether to plot error bars (representing the 95 percent confidence interval for the mean of each variable)
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#' @examples
#' m3 <- estimate_profiles(iris,
#'     Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'     model = 1,
#'     n_profiles = 3)
#' plot_profiles(m3)
#' @export

plot_profiles <- function(x, to_center = F, to_scale = F, plot_what = "tibble", plot_error_bars = TRUE) {
  if (plot_what == "tibble") {
    n <- count(x, .data$profile)
    x <- mutate(x, profile = factor(
      .data$profile,
      labels = paste0("Profile ", n$profile, " (n = ", n$n, ")")
    ))

    if (plot_error_bars == TRUE) {
      x %>%
        select(-.data$posterior_prob) %>%
        mutate_at(vars(-.data$profile), center_scale_function, center_raw_data = to_center, scale_raw_data = to_scale) %>%
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
        ) %>%
        ggplot(aes_string(x = "profile", y = "mean", fill = "key", ymin = "ymin", ymax = "ymax")) +
        geom_col(position = "dodge") +
        geom_errorbar(position = position_dodge()) +
        scale_fill_brewer("", type = "qual", palette = 6) +
        scale_x_discrete("") +
        theme_bw()
    } else {
      x %>%
        dplyr::select(-.data$posterior_prob) %>%
        dplyr::mutate_at(vars(-.data$profile), scale, center = to_center, scale = to_scale) %>%
        dplyr::mutate(profile = as.factor(.data$profile)) %>%
        group_by(.data$profile) %>%
        summarize_all(mean) %>%
        tidyr::gather("key", "val", -.data$profile) %>%
        ggplot(aes_string(x = "profile", y = "val", fill = "key")) +
        geom_col(position = "dodge") +
        scale_fill_brewer("", type = "qual", palette = 6) +
        scale_x_discrete("") +
        theme_bw()
    }
  } else if (plot_what == "mclust") {
    stop("cannot presently plot mclust objects")
  }
}
