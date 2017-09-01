# functions.R

#' Explore BIC of mclust models
#' @details Explore the BIC values of a range of models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param df data.frame with two or more columns with continuous variables
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 1 through 9 (1:9)
#' @param model_names mclust models to explore; defaults to constrained variance, fixed variances ("EII"), constrained variance, constrained covariance ("EEE"), and freed variance, freed covariance ("VVV"); run mclust::mclustModelNames() to see all of the possible models and their names / abbreviations
#' @return a ggplot2 plot of the BIC values for the explored models
#' @examples
#' library(dplyr)
#' df <- select(iris, -Species)
#' explore_models_mclust(df)
#' @export

explore_models_mclust <- function(df, n_profiles_range = 1:9, model_names = c("EEI", "EEE", "VVV"), statistic = "BIC", return_table = FALSE) {

    if (statistic == "BIC") {
        x <- mclust::mclustBIC(df, G = n_profiles_range, modelNames = model_names)
    } else if (statistic == "ICL") {
        x <- mclust::mclustICL(df, G = n_profiles_range, modelNames = model_names)
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
        ggplot2::ylab(paste0(statistic, " (smaller value is better)"))

}

#' Create profiles for a specific mclust model
#' @details Creates profiles (or estimates the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param variance_structure specification for residual variances, either "constrained" (to be equivalent across profiles) or "freed" (to be freely-estimated across profiles)
#' @param covariance_structure specification for residual variance, either "fixed" (to be zero inall profiles), "constrained" (to be equivalent across profiles), or "freed" (to be freely-estimated across profiles)
#' @param model_name the mclust model to explore, such as constrained variance, fixed variances ("EII"), constrained variance, constrained covariance ("EEE"), and freed variance, freed covariance ("VVV"); or others (run mclust::mclustModelNames() to see all of the possible models and their names / abbreviations); if specified, arguments variance_structure and covariance_structure are ignored
#' @return a ggplot2 plot of the BIC values for the explored models
#' @importFrom magrittr '%>%'
#' @importFrom mclust 'mclustBIC'
#' @examples
#' library(dplyr)
#' df <- select(iris, -Species)
#' create_profiles_mclust(df, n_profiles = 3, variance_structure = "freed", covariance_structure = "freed")
#' @export

create_profiles_mclust <- function(df,
                                   n_profiles,
                                   variance_structure = "freed",
                                   covariance_structure = "freed",
                                   model_name = NULL){

    if (is.null(model_name)) {

        if (variance_structure == "constrained" & covariance_structure == "fixed") {

            model_name <- "EEI"

        } else if (variance_structure == "freed" & covariance_structure == "fixed") {

            model_name <- "VVI"

        } else if (variance_structure == "constrained" & covariance_structure == "constrained") {

            model_name <- "EEE"

        } else if (variance_structure == "freed" & covariance_structure == "freed") {

            model_name <- "VVV"

        } else if (variance_structure == "fixed") {

            stop("variance_structure cannot equal 'fixed' using this function; change this to 'constrained' or 'freed' or try one of the models from mclust::Mclust()")

        }
    }

    print(model_name)

    x <- mclust::Mclust(df, G = n_profiles, modelNames = model_name)

    dff <- dplyr::bind_cols(df, classification = x$classification)

    attributes(dff)$mclust_output <- x

    return(dff)

    # proc_df <- dff %>%
    #     dplyr::mutate_at(vars(-classification), scale) %>%
    #     dplyr::group_by(classification) %>%
    #     dplyr::summarize_all(funs(mean)) %>%
    #     dplyr::mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    #     dplyr::mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    #     dplyr::rename(profile = classification)


    # return(proc_df)
}

#' Extract mclust output from the function create_profiles_mclust()
#' @details Extract the output of the mclust output from the function create_profiles_mclust() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_output <- function(x) {
    attributes(x)$mclust_output
}

#' Extract mclust classifications from the function create_profiles_mclust()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles_mclust() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classifications <- function(x) {
    attributes(x)$mclust_output$classification
}

#' Extract mclust classifications from the function create_profiles_mclust()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles_mclust() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classification_certainty <- function(x) {
    1 - attributes(x)$mclust_output$uncertainty
}

#' Bootstrap the likelihood-ratio test statistic for mixture components
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model
#' @param df data.frame with two or more columns with continuous variables
#' @param model_names names of one or more models ?run mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @importFrom magrittr '%>%'
#' @export

bootstrap_LRT_mclust <- function(df, model_names, ...) {
    if (length(model_names) == 1) {
        mclust::mclustBootstrapLRT(data = df, modelName = model_names, ...)
    } else if (length(model_names) > 1) {
        model_names %>% purrr::map(~ mclust::mclustBootstrapLRT(data = df, modelName = .))
    }
}
