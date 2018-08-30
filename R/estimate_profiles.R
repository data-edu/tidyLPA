#' Estimate parameters for profiles for a specific solution
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param variances how the variable variances are estimated; defaults to "equal" (to be constant across profiles); other option is "varying" (to be varying across profiles)
#' @param covariances how the variable covariances are estimated; defaults to "zero" (to not be estimated, i.e. for the covariance matrix to be diagonal); other options are "varying" (to be varying across profiles) and "equal" (to be constant across profiles)
#' @param model which model to estimate (DEPRECATED; use variances and covariances instead)
#' @param center_raw_data logical for whether to center (M = 1) the raw data (before clustering); defaults to FALSE
#' @param scale_raw_data logical for whether to scale (SD = 1) the raw data (before clustering); defaults to FALSE
#' @param to_return character string for either "tibble" (or "data.frame") or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to TRUE
#' @param return_orig_df TRUE or FALSE (if TRUE, then the entire data.frame is returned; if FALSE, then only the variables used in the model are returned)
#' @param prior_control whether to include a regularizing prior; defaults to false
#' @param print_which_stats if set to "some", prints (as a message) the log-likelihood, BIC, and entropy; if set to "all", prints (as a message) all information criteria and other statistics about the model; if set to any other values, then nothing is printed
#' @inheritParams compare_solutions_mplus
#' @examples
#' estimate_profiles(iris,
#'     Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'     n_profiles = 3)
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

estimate_profiles <- function(df,
                              ...,
                              n_profiles,
                              variances = "equal",
                              covariances = "zero",
                              to_return = "tibble",
                              model = NULL,
                              center_raw_data = FALSE,
                              scale_raw_data = FALSE,
                              return_posterior_probs = TRUE,
                              return_orig_df = FALSE,
                              prior_control = FALSE,
                              print_which_stats = "some") {

    model_message <- c("The model command is deprecated in favor of the arguments for the variances and covariances. The models correspond to the following arguments for the variances and covariances:
Model 1: variances = 'equal'; covariances = 'zero';
Model 2: variances = 'equal'; covariances = 'equal';
Model 3: variances = 'equal'; covariances = 'zero';
Model 4: variances = 'varying'; covariances = 'equal' (Cannot be estimated without MPlus);
Model 5: variances = 'equal'; covariances = 'varying' (Cannot be estimated without MPlus);
Model 6: variances = 'varying'; covariances = 'varying';
                     ")

    if (!is.null(model)) stop(model_message)

    if ("row_number" %in% names(df)) warning("existing variable in df 'row_number' will be overwritten")

    df <- mutate(df, row_number = 1:nrow(df))

    d <- select_create_profiles(df, ...)

    if (center_raw_data == T | scale_raw_data == T) {
        d <- mutate_at(d, vars(-row_number), center_scale_function, center_raw_data = center_raw_data, scale_raw_data = scale_raw_data)
    }

    if (variances == "equal" & covariances == "zero") {
        model <- "EEI"
    } else if (variances == "equal" & covariances == "equal") {
        model <- "EEE"
    } else if (variances == "varying" & covariances == "zero") {
        model <- "VVI"
    } else if (variances == "varying" & covariances == "varying") {
        model <- "VVV"
    } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
        model <- model
    } else {
        stop("Model name is not correctly specified")
    }

    model_number <- case_when(
        variances == "equal" & covariances == "zero" ~ 1,
        variances == "varying" & covariances == "zero" ~ 2,
        variances == "equal" & covariances == "equal" ~ 3,
        variances == "varying" & covariances == "equal" ~ 4,
        variances == "equal" & covariances == "varying" ~ 5,
        variances == "varying" & covariances == "varying" ~ 6
    )

    titles <- c(
        "Equal variances and covariances fixed to 0 (model 1)",
        "Varying variances and covariances fixed to 0 (model 2)",
        "Equal variances and equal covariances (model 3)",
        #"Varying variances and equal covariances (model 4)",
        #"Equal variances and varying covariances (model 5)",
        "Varying variances and varying covariances (model 6)"
    )

    title <- titles[model_number]

    d_model <- select(d, -row_number)

    if (prior_control == FALSE) {
        m <- Mclust(d_model, G = n_profiles, modelNames = model, warn = FALSE, verbose = FALSE)
    } else {
        m <- Mclust(d_model, G = n_profiles, modelNames = model, warn = FALSE, verbose = FALSE, prior = priorControl())
    }

    if (is.null(m)) stop("Model could not be fitted")

    message("Fit ", titles[model], " model with ", n_profiles, " profiles.")

    AIC <- (2 * m$df - 2 * m$loglik)
    # BIC1 <- (m$df * log(m$n) - 2 * m$loglik) # same as BIC output from mclust
    CAIC <- (((log(m$n) + 1) * m$df) - 2 * m$loglik)
    SABIC <- (m$df * log((m$n + 2) / 24) - 2 * m$loglik)

    posterior_prob <- 1 - round(m$uncertainty, 5)

    if (tolower(print_which_stats) == "some") {
        message("LogLik is ", round(abs(as.vector(m$loglik)), 3))
        message("BIC is ", round(abs(as.vector(m$BIC)), 3))
        message("Entropy is ", round(mean(posterior_prob), 3))
    } else if (tolower(print_which_stats) == "all") {
        message("LogLik is ", round(abs(as.vector(m$loglik)), 3))
        message("AIC is ", round(abs(as.vector(AIC)), 3))
        message("CAIC is ", round(abs(as.vector(CAIC)), 3))
        message("BIC is ", round(abs(as.vector(m$BIC)), 3))
        message("SABIC is ", round(abs(as.vector(SABIC)), 3))
        message("ICL is ", round(abs(as.vector(icl(m))), 3))
        message("Entropy is ", round(mean(posterior_prob), 3))
    }

    dff <- as.data.frame(bind_cols(d, profile = as.factor(m$classification))) # replace with tibble as bind_cols acts up

    test <- nrow(count(dff, .data$profile))

    if (test < n_profiles) warning("Some profiles are associated with no assignments. Interpret this solution with caution and consider other models.")

    if (return_posterior_probs == TRUE) {
        dff <- bind_cols(dff, posterior_prob = posterior_prob)
    }

    attributes(dff)$mclust_output <- m

    if (return_orig_df == TRUE) {
        to_join <- select(dff, .data$profile, .data$posterior_prob)
        dff <- semi_join(df, dff, by = "row_number")
        dff <- select(dff, -.data$row_number)
        dff <- bind_cols(dff, to_join)
    } else {
        dff <- semi_join(dff, df, by = "row_number")
        dff <- select(dff, -.data$row_number)
    }

    if (to_return == "tibble" | to_return == "data.frame") {
        return(as_tibble(dff))
    } else if (to_return == "mclust") {
        return(attributes(dff)$mclust_output)
    }
}
