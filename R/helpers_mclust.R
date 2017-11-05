#' Calculate centroids from an mclust model object
#' @details Extract the centroid values of an mclust model object
#' @param x an object of class `Mclust`
#' @export

calculate_centroids_mclust <- function(x) {
    x <- tibble::rownames_to_column(as.data.frame(x$parameters$mean))
    names(x) <- c("Variable", paste0("Profile", 1:x$G))
    x
}

#' Extract mclust output from an mclust model object
#' @details Extract the output of the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @inheritParams calculate_centroids_mclust
#' @export

extract_mclust_output <- function(x) {
    attributes(x)$mclust_output
}

#' Extract mclust classifications from an mclust model object
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @inheritParams calculate_centroids_mclust
#' @export

extract_mclust_classifications <- function(x) {
    attributes(x)$mclust_output$classification
}

#' Extract mclust classifications from the function create_profiles()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @inheritParams calculate_centroids_mclust
#' @export

extract_mclust_classification_certainty <- function(x) {
    1 - attributes(x)$mclust_output$uncertainty
}

#' Extract mclust variances
#' @details Extract the variances and covariances
#' @inheritParams calculate_centroids_mclust
#' @param profile_n the number of profiles
#' @importFrom dplyr %>%
#' @export

extract_variance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        diag() %>%
        dplyr::as_tibble() %>%
        dplyr::rename("est" = .data$value) %>%
        tibble::rownames_to_column("var_name") %>%
        dplyr::mutate(param_name = "Variances") %>%
        dplyr::mutate(class = paste0("class_", .data$profile_n),
                      est = round(.data$est, 3)) %>%
        dplyr::select(.data$param_name, .data$var_name, .data$class, .data$est)
}

#' Extract mclust covariance
#' @details Extract the covariances
#' @inheritParams calculate_centroids_mclust
#' @param profile_n the number of profiles
#' @import dplyr
#' @export

extract_covariance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        as.data.frame() %>%
        tibble::rownames_to_column("param_name") %>%
        as.tibble() %>%
        tidyr::gather("key", "val", -.data$param_name) %>%
        rename(var_name = .data$key, est = .data$val) %>%
        mutate(param_name = toupper(stringr::str_sub(.data$param_name, start = 1L, end = 8L)),
               param_name = paste0(.data$param_name, ".WITH"),
               param_name = stringr::str_replace(.data$param_name, "\\.", "_"),
               var_name = toupper(.data$var_name),
               var_name = stringr::str_replace(.data$var_name, "\\.", "_"),
               var_name = stringr::str_sub(.data$var_name, start = 1L, end = 10L))
}

#' Extract mclust summary statistics
#' @details Extract the log likelihood, BIC, and entropy statistics
#' @inheritParams calculate_centroids_mclust
#' @export

extract_mclust_summary <- function(x) {
    data.frame(LL = round(x$loglik, 3),
               BIC = round(x$bic * -1, 3),
               Entropy = round(1 - mean(round(x$uncertainty, 3)), 3))
}
