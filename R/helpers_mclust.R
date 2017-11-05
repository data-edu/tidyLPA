#' Extract mclust variances
#' @details Extract the variances and covariances
#' @param x object of class Mclust
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export

extract_variance <- function(x) {
    profile_n <- x$G
    x$parameters$variance$sigma[, , profile_n] %>%
        diag() %>%
        dplyr::as_tibble() %>%
        dplyr::rename("est" = .data$value) %>%
        tibble::rownames_to_column("var_name") %>%
        dplyr::mutate(param_name = "Variances") %>%
        dplyr::mutate(class = paste0("class_", profile_n),
                      est = round(.data$est, 3)) %>%
        dplyr::select(.data$param_name, .data$var_name, .data$class, .data$est)
}

#' Extract mclust covariances
#' @details Extract the covariances
#' @param x object of class Mclust
#' @import dplyr
#' @importFrom rlang .data
#' @export

extract_covariance <- function(x) {
    profile_n <- x$G
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
#' @param x object of class Mclust
#' @export

extract_mclust_summary <- function(x) {
    data.frame(LL = round(x$loglik, 3),
               BIC = round(x$bic * -1, 3),
               Entropy = round(1 - mean(round(x$uncertainty, 3)), 3))
}
