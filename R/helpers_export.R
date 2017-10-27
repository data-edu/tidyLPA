# helpers_export

#' student questionnaire data with four variables from the 2015 PISA for students in the United States
#'
#' @source http://www.oecd.org/pisa/data/
#' @format Data frame with columns
#' #' \describe{
#'   \item{CNTSTUID}{international student ID}
#'   \item{SCHID}{international school ID}
#'   ...
#' }
#' @import tibble

"pisaUSA15"

#' Calculate centroids from the output of create_profiles_lpa()
#' @details Extract the output of create_profiles_lpa()) so that statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

calculate_centroids_mclust <- function(x) {
    o <- tibble::rownames_to_column(as.data.frame(x$parameters$mean))
    names(o) <- c("Variable", paste0("Profile", 1:x$G))
    o
}

# proc_df <- dff %>%
#     dplyr::mutate_at(vars(-classification), scale) %>%
#     dplyr::group_by(classification) %>%
#     dplyr::summarize_all(funs(mean)) %>%
#     dplyr::mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
#     dplyr::mutate_at(vars(-classification), function(x) round(x, 3)) %>%
#     dplyr::rename(profile = classification)

#' Extract mclust output from the function create_profiles()
#' @details Extract the output of the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_output <- function(x) {
    attributes(x)$mclust_output
}

#' Extract mclust classifications from the function create_profiles()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classifications <- function(x) {
    attributes(x)$mclust_output$classification
}

#' Extract mclust classifications from the function create_profiles()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classification_certainty <- function(x) {
    1 - attributes(x)$mclust_output$uncertainty
}

#' Calculate summary statistics for mclust output
#' @details groups by profile and calculates the mean
#' @param x output from create_profiles_mclust() when to_return is set to TRUE
#' @import dplyr
#' @importFrom magrittr %>%
#' @export

summary_mclust <- function(x) {
    x %>%
        group_by(profile) %>%
        summarize_all(mean)
}

#' Bootstrap the likelihood-ratio test statistic for mixture components
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model
#' @param df data.frame with two or more columns with continuous variables
#' @param ... additional arguments to be passed to mclustBootstrapLRT()
#' @param model_names names of one or more models ?run mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @export

bootstrap_LRT_mclust <- function(df, model_names, ...) {
    if (length(model_names) == 1) {
        mclust::mclustBootstrapLRT(data = df, modelName = model_names, ...)
    } else if (length(model_names) > 1) {
        purrr::map(model_names, ~ mclust::mclustBootstrapLRT(data = df, modelName = .))
    }
}

#' Extract mclust variances
#' @details Extract the variances and covariances
#' @param x an object of class `Mclust`
#' @param profile_n the number of profiles
#' @export

extract_variance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        diag() %>%
        dplyr::as_tibble() %>%
        dplyr::rename(est = value) %>%
        tibble::rownames_to_column("var_name") %>%
        dplyr::mutate(param_name = "Variances") %>%
        dplyr::mutate(class = paste0("class_", profile_n),
                      est = round(est, 3)) %>%
        dplyr::select(param_name, var_name, class, est)
}

#' Extract mclust covariance
#' @details Extract the covariances
#' @param x an object of class `Mclust`
#' @param profile_n the number of profiles
#' @import dplyr
#' @export

extract_covariance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        as.data.frame() %>%
        rownames_to_column("param_name") %>%
        as.tibble() %>%
        gather(key, val, -param_name) %>%
        rename(var_name = key, est = val) %>%
        mutate(param_name = toupper(str_sub(param_name, start = 1L, end = 8L)),
               param_name = paste0(param_name, ".WITH"),
               param_name = str_replace(param_name, "\\.", "_"),
               var_name = toupper(var_name),
               var_name = str_replace(var_name, "\\.", "_"),
               var_name = str_sub(var_name, start = 1L, end = 10L))
}

#' Extract mclust summary statistics
#' @details Extract the log likelihood, BIC, and entropy statistics
#' @param x an object of class `Mclust`
#' @export

extract_mclust_summary <- function(x) {
    data.frame(LL = round(x$loglik, 3),
               BIC = round(x$bic * -1, 3),
               Entropy = round(1 - mean(round(x$uncertainty, 3)), 3))
}
