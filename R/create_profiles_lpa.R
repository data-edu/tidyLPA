select_create_profiles <- function(df, ...){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ..., row_number)

    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep

    # cases_to_keep <- dplyr::data_frame(row_names = 1:nrow(df_ss),
    #                                    keep = cases_to_keep)

    d <- df_ss[cases_to_keep, ] # removes incomplete cases

    return(d)
}

#' Create profiles for a specific mclust model
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param model the mclust model to explore: 1 (varying means, equal variances, and residual covariances fixed to zero); 2 (varying means, equal variances and covariances; and 3 (varying means, variances, and covariances), in order from most to least constrained; run ?mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @param to_return character string for either "tibble" or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to TRUE
#' @import mclust
#' @examples
#' d <- pisaUSA15
#' d <- dplyr::sample_n(d, 200)
#' m3 <- create_profiles_lpa(d,
#'                           broad_interest, enjoyment, self_efficacy,
#'                           n_profiles = 3, to_return="tibble")
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

create_profiles_lpa <- function(df,
                                ...,
                                n_profiles,
                                model = 1,
                                to_return = "tibble",
                                return_posterior_probs = TRUE){

    if ("row_number" %in% names(df)) warning("existing variable in df 'row_number' will be overwritten")

    df <- dplyr::mutate(df, row_number = 1:nrow(df))

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

    d_model <- dplyr::select(d, -row_number)

    m <- mclust::Mclust(d_model, G = n_profiles, modelNames = model)

    if(is.null(m)) stop("Model could not be fitted")

    message("Model with ", n_profiles, " profiles using the '", model_print, "' model.")

    AIC <- (2*m$df - 2*m$loglik)
    posterior_prob <- 1 - round(m$uncertainty, 5)

    message("AIC is ", round(abs(as.vector(AIC)), 3))
    message("BIC is ", round(abs(as.vector(m$BIC)), 3))
    message("ICL is ", round(abs(as.vector(mclust::icl(m))), 3))
    message("Entropy is ", round(mean(posterior_prob), 5))

    dff <- as.data.frame(dplyr::bind_cols(d, profile = m$classification)) # replace with tibble as bind_cols acts up

    if (return_posterior_probs == TRUE) {
        dff <- dplyr::bind_cols(dff, posterior_prob = posterior_prob)
    }

    attributes(dff)$mclust_output <- m

    dff <- dplyr::semi_join(dff, df, by = "row_number")
    dff <- dplyr::select(dff, -row_number)

    if (to_return == "tibble") {
        return(tibble::as_tibble(dff))
    } else if (to_return == "mclust") {
        return(attributes(dff)$mclust_output)
    }
}
