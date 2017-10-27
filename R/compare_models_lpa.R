
#' Explore BIC of mclust models
#' @details Explore the BIC values of a range of models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 1 through 9 (1:9)
#' @param statistic what statistic to plot; BIC or ICL are presently available as options
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output intsead of a plot; defaults to FALSE
#' @inheritParams create_profiles_lpa
#' @return a ggplot2 plot of the BIC values for the explored models
#' @import mclust
#' @examples
#' d <- iris
#' compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' @export

compare_models_lpa <- function(df, ..., n_profiles_range = 1:9, model = c(1, 2, 3), statistic = "BIC", return_table = FALSE) {

    d <- select_create_profiles(df, ...)

    model <- dplyr::case_when(
        model == 1 ~ "EEI",
        model == 2 ~ "EEE",
        model == 3 ~ "VVV",
        TRUE ~ as.character(model)
    )

    if (statistic == "BIC") {
        x <- mclust::mclustBIC(d, G = n_profiles_range, modelNames = model)
    } else if (statistic == "ICL") {
        x <- mclust::mclustICL(d, G = n_profiles_range, modelNames = model_names)
    } else {
        stop("This statistic cannot presently be computed")
    }

    y <- x %>%
        as.data.frame.matrix() %>%
        tibble::rownames_to_column("n_profiles") %>%
        dplyr::rename(`Constrained variance, fixed covariance` = EEI,
                      `Constrained variance, constrained covariance` = EEE,
                      `Freed variance, freed covariance` = VVV)

    to_plot <- y %>%
        tidyr::gather(`Covariance matrix structure`, val, -n_profiles) %>%
        dplyr::mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
                      val = abs(val)) # this is to make the BIC values positive (to align with more common formula / interpretation of BIC)

    to_plot$`Covariance matrix structure` <- forcats::fct_relevel(to_plot$`Covariance matrix structure`,
                                                                  "Constrained variance, fixed covariance",
                                                                  "Constrained variance, constrained covariance",
                                                                  "Freed variance, freed covariance")


    if(return_table == TRUE) {
        return(to_plot)
    }

    ggplot2::ggplot(to_plot, ggplot2::aes(x = n_profiles, y = val, color = `Covariance matrix structure`, group = `Covariance matrix structure`)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::ylab(paste0(statistic, " (smaller value is better)")) +
        ggplot2::theme_bw()

}
