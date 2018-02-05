#' Estimate profiles for a specific mclust model
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param model the mclust model to explore: 1 (varying means, equal variances, and residual covariances fixed to 0); 2 (varying means, equal variances and covariances; 3 (varying means and variances, covariances fixed to 0), 4 (varying means and covariances, equal variances; can only be specified in Mplus); 5 (varying means, equal variances, varying covariances); and 6 (varying means, variances, and covariances), in order least to most freely-estimated; see the introductory vignette for more information
#' @param center_raw_data logical for whether to center (M = 1) the raw data (before clustering); defaults to FALSE
#' @param scale_raw_data logical for whether to scale (SD = 1) the raw data (before clustering); defaults to FALSE
#' @param to_return character string for either "tibble" (or "data.frame") or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to TRUE
#' @param return_orig_df TRUE or FALSE (if TRUE, then the entire data.frame is returned; if FALSE, then only the variables used in the model are returned)
#' @param prior_control whether to include a regularizing prior; defaults to false
#' @param print_which_stats if set to "some", prints (as a message) the log-likelihood, BIC, and entropy; if set to "all", prints (as a message) all information criteria and other statistics about the model; if set to any other values, then nothing is printed
#' @import mclust
#' @importFrom rlang .data
#' @examples
#' estimate_profiles(iris,
#'     Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'     model = 1,
#'     n_profiles = 3)
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

estimate_profiles <- function(df,
                              ...,
                              n_profiles,
                              model = 1,
                              to_return = "tibble",
                              center_raw_data = FALSE,
                              scale_raw_data = FALSE,
                              return_posterior_probs = TRUE,
                              return_orig_df = FALSE,
                              prior_control = FALSE,
                              print_which_stats = "some") {
  if ("row_number" %in% names(df)) warning("existing variable in df 'row_number' will be overwritten")

  df <- dplyr::mutate(df, row_number = 1:nrow(df))

  d <- select_create_profiles(df, ...)

  if (center_raw_data == T | scale_raw_data == T) {
    d <- mutate_at(d, vars(-row_number), center_scale_function, center_raw_data = center_raw_data, scale_raw_data = scale_raw_data)
  }

  if (model == 1) {
    model <- "EEI"
  } else if (model == 2) {
    model <- "EEE"
  } else if (model == 3) {
    model <- "VVI"
  } else if (model == 6) {
    model <- "VVV"
  } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
    model <- model
  } else {
    stop("Model name is not correctly specified: use 1, 2, 3, or 6 (see ?estimate_profiles for descriptions) or one of the model names specified from mclustModelNames() from mclust")
  }

  model_print <- ifelse(model == "EEI", "varying means, equal variances, covariances fixed to 0 (Model 1)",
    ifelse(model == "EEE", "varying means, equal variances and covariances (Model 2)",
      ifelse(model == "VVI", "varying means and variances, covariances fixed to 0 (Model 3)",
        ifelse(model == "VVV", "varying means, variances, and covariances (Model 4)", model)
      )
    )
  )

  d_model <- dplyr::select(d, -row_number)

  if (prior_control == FALSE) {
    m <- mclust::Mclust(d_model, G = n_profiles, modelNames = model, warn = FALSE, verbose = FALSE)
  } else {
    m <- mclust::Mclust(d_model, G = n_profiles, modelNames = model, warn = FALSE, verbose = FALSE, prior = mclust::priorControl())
  }

  if (is.null(m)) stop("Model could not be fitted")

  message("Fit ", model_print, " model with ", n_profiles, " profiles.")

  AIC <- (2 * m$df - 2 * m$loglik)
  # BIC1 <- (m$df * log(m$n) - 2 * m$loglik) # same as BIC output from mclust
  CAIC <- (((log(m$n) + 1) * m$df) - 2 * m$loglik)
  SABIC <- (m$df * log((m$n + 2) / 24) - 2 * m$loglik)

  posterior_prob <- 1 - round(m$uncertainty, 5)

  if (tolower(print_which_stats) == "some") {
      message("LogLik is ", round(abs(as.vector(m$loglik)), 3))
      message("BIC is ", round(abs(as.vector(m$BIC)), 3))
      message("Entropy is ", round(mean(posterior_prob), 3))
  } else if (tolower(print_which_stats) == "all"){
      message("LogLik is ", round(abs(as.vector(m$loglik)), 3))
      message("AIC is ", round(abs(as.vector(AIC)), 3))
      message("CAIC is ", round(abs(as.vector(CAIC)), 3))
      message("BIC is ", round(abs(as.vector(m$BIC)), 3))
      message("SABIC is ", round(abs(as.vector(SABIC)), 3))
      message("ICL is ", round(abs(as.vector(mclust::icl(m))), 3))
      message("Entropy is ", round(mean(posterior_prob), 3))
  }

  dff <- as.data.frame(dplyr::bind_cols(d, profile = as.factor(m$classification))) # replace with tibble as bind_cols acts up

  test <- nrow(dplyr::count(dff, .data$profile))

  if (test < n_profiles) warning("Some profiles are associated with no assignments. Interpret this solution with caution and consider other models.")

  if (return_posterior_probs == TRUE) {
    dff <- dplyr::bind_cols(dff, posterior_prob = posterior_prob)
  }

  attributes(dff)$mclust_output <- m

  if (return_orig_df == TRUE) {
    to_join <- dplyr::select(dff, .data$profile, .data$posterior_prob)
    dff <- dplyr::semi_join(df, dff, by = "row_number")
    dff <- dplyr::select(dff, -.data$row_number)
    dff <- bind_cols(dff, to_join)
  } else {
    dff <- dplyr::semi_join(dff, df, by = "row_number")
    dff <- dplyr::select(dff, -.data$row_number)
  }

  if (to_return == "tibble" | to_return == "data.frame") {
    return(tibble::as_tibble(dff))
  } else if (to_return == "mclust") {
    return(attributes(dff)$mclust_output)
  }
}
