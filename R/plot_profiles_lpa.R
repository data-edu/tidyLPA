
#' Plot profile centroids
#' @details Plot the centroids for tibble or mclust output from create_profiles_lpa()
#' @param x output from create_profiles_mclust()
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @param plot_what whether to plot tibble or mclust output from create_profiles_lpa(); defaults to tibble
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'

plot_profiles_lpa <- function(x, to_center = F, to_scale = F, plot_what = "tibble") {

    if (plot_what == "tibble") {

        x %>%
            dplyr::select(-.data$posterior_prob) %>%
            dplyr::mutate_at(vars(-.data$profile), scale, center = to_center, scale = to_scale) %>%
            group_by(.data$profile) %>%
            summarize_all(mean) %>%
            tidyr::gather("key", "val", -.data$profile) %>%
            ggplot(aes_string(x = "profile", y = "val", fill = "key")) +
            geom_col(position = "dodge") +
            scale_fill_brewer("", type = "qual", palette=6) +
            theme_bw()

    } else if (plot_what == "mclust") {

    }

}
