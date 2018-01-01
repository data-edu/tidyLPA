
#' Explore BIC of mclust models
#' @details Explore the BIC values of a range of models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 1 through 9 (1:9)
#' @param statistic what statistic to plot; BIC or ICL are presently available as options
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to FALSE
#' @inheritParams create_profiles_lpa
#' @return a ggplot2 plot of the BIC values for the explored models
#' @import mclust
#' @examples
#' d <- iris
#' compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' @export

compare_models_lpa <- function(df, ..., n_profiles_range = 1:9, model = c(1, 2, 3, 5), center_raw_data = FALSE, scale_raw_data = FALSE, statistic = "BIC", return_table = FALSE, prior_control = F) {

    d <- select_ancillary_functions(df, ...)

    if (center_raw_data == T | scale_raw_data == T) {
        d <- mutate_all(d, center_scale_function, center_raw_data = center_raw_data, scale_raw_data = scale_raw_data)
    }

    model <- dplyr::case_when(
        model == 1 ~ "EEI",
        model == 2 ~ "EEE",
        model == 3 ~ "VVI",
        model == 5 ~ "VVV",
        TRUE ~ as.character(model)
    )

    if(prior_control == FALSE) {
        if (statistic == "BIC") {
            x <- mclust::mclustBIC(d, G = n_profiles_range, modelNames = model, warn = TRUE, verbose = FALSE)
        } else if (statistic == "ICL") {
            x <- mclust::mclustICL(d, G = n_profiles_range, modelNames = model, warn = TRUE, verbose = FALSE)
        } else {
            stop("This statistic cannot presently be computed")
        }
    } else {
        if (statistic == "BIC") {
            x <- mclust::mclustBIC(d, G = n_profiles_range, modelNames = model, warn = TRUE, verbose = FALSE, prior = mclust::priorControl())
        } else if (statistic == "ICL") {
            x <- mclust::mclustICL(d, G = n_profiles_range, modelNames = model, warn = TRUE, verbose = FALSE, prior = mclust::priorControl())
        } else {
            stop("This statistic cannot presently be computed")
        }
    }


    y <- x %>%
        as.data.frame.matrix() %>%
        tibble::rownames_to_column("n_profiles") %>%
        dplyr::rename(`Varying means, equal variances, covariances fixed to 0 (Model 1)` = "EEI",
                      `Varying means, equal variances and covariances (Model 2)` = "EEE",
                      `Varying means and variances, covariances fixed to 0 (Model 3)` = "VVI",
                      `Varying means, variances, and covariances (Model 5)` = "VVV")

    to_plot <- y %>%
        tidyr::gather("Covariance matrix structure", "val", -.data$n_profiles) %>%
        dplyr::mutate("Covariance matrix structure" = as.factor(.data$`Covariance matrix structure`),
                      val = abs(.data$val)) # this is to make the BIC values positive (to align with more common formula / interpretation of BIC)

    to_plot$`Covariance matrix structure` <- forcats::fct_relevel(to_plot$`Covariance matrix structure`,
                                                                  "Varying means, equal variances, covariances fixed to 0 (Model 1)",
                                                                  "Varying means, equal variances and covariances (Model 2)",
                                                                  "Varying means and variances, covariances fixed to 0 (Model 3)",
                                                                  "Varying means, variances, and covariances (Model 5)")


    if(return_table == TRUE) {
        return(to_plot)
    }

    to_plot$n_profiles <- as.factor(to_plot$n_profiles)

    ggplot2::ggplot(to_plot, ggplot2::aes_string(x = "n_profiles",
                                                 y = "val",
                                                 color = "`Covariance matrix structure`",
                                                 group = "`Covariance matrix structure`")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::ylab(paste0(statistic, " (smaller value is better)")) +
        ggplot2::theme_bw()

}
