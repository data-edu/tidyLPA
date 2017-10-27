
#' Create profiles for a specific mclust model
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param model the mclust model to explore: 1 (varying means, equal variances, and residual covariances fixed to zero); 2 (varying means, equal variances and covariances; and 3 (varying means, variances, and covariances), in order from most to least constrained; run ?mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @param to_return character string for either "tibble" or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to FALSE
#' @import mclust
#' @examples
#' \dontrun{
#' d <- pisaUSA15
#' m3 <- create_profiles_lpa(d,
#'                              broad_interest, enjoyment, instrumental_mot, self_efficacy,
#'                              n_profiles = 3, to_return="tibble")
#' }
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

create_profiles_lpa <- function(df,
                                ...,
                                n_profiles,
                                model = 1,
                                to_return = "tibble",
                                return_posterior_probs = TRUE){

    d <- select_create_profiles(df, ...)

    if (model == 1) {
        model <- "EEI"
    } else if (model == 2) {
        model <- "EEE"
    } else if (model == 3) {
        model <- "VVV"
    } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
        model <- model
    } else {
        stop("Model name is not correctly specified: use 1, 2, or 3 (see ?create_profiles_lpa for descriptions) or one of the model names specified from mclustModelNames() from mclust")
    }

    model_print <- ifelse(model == "EEI", "varying means, equal variances, and residual covariances fixed to zero",
                          ifelse(model == "EEE", "varying means, equal variances and covariances",
                                 ifelse(model == "VVV", "varying means, variances, and covariances", model)))

    m <- mclust::Mclust(d, G = n_profiles, modelNames = model)

    message("Model with ", n_profiles, " profiles using the '", model_print, "' model.")

    AIC <- (2*m$df - 2*m$loglik)

    message("Model AIC is ", round(abs(as.vector(AIC)), 3))
    message("Model BIC is ", round(abs(as.vector(m$BIC)), 3))
    message("Model ICL is ", round(abs(as.vector(mclust::icl(m))), 3))

    dff <- as.data.frame(dplyr::bind_cols(d, profile = m$classification)) # replace with tibble

    if (return_posterior_probs == TRUE) {
        col <- 1 - round(m$uncertainty, 5)
        dff <- dplyr::bind_cols(dff, posterior_prob = col)
    }

    attributes(dff)$mclust_output <- m

    if (to_return == "tibble") {
        return(tibble::as_tibble(dff))
    } else if (to_return == "mclust") {
        return(attributes(dff)$mclust_output)
    }
}
