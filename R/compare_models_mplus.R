#' Explore BIC of MPlus models
#' @details Explore the BIC values of a range of Mplus models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 2 through 10 (2:10)
#' @inheritParams compare_models_lpa
#' @return a list with a data.frame with the BIC values and a list with all of the model output; if save_models is the name of an rds file (i.e., "out.rds"), then the model output will be written with that filename and only the data.frame will be returned
#' @import mclust
#' @examples
#' compare_models_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' @export

compare_models_mplus <- function(df, ..., n_profiles = 2:10, models = 1:5, start_iterations = c(100, 20), m_iterations = 500,
                                 save_models = NULL) {
    out_df <- data.frame(matrix(ncol = length(models), nrow = length(n_profiles)))
    names(out_df) <- paste0("model_", models)
    out_df <- out_df %>%
        mutate(n_profiles = n_profiles) %>%
        select(n_profiles, everything())

    out_list <- as.list(rep(NA, times = (nrow(out_df) * ncol(out_df))))

    for (i in n_profiles) {
        for (j in models) {
            m <- create_profiles_mplus(df, ..., n_profiles = i, model = j, start_iterations = start_iterations, m_iterations = m_iterations)
            the_index <- sum(!is.na(out_list))
            message(paste0("Model ", the_index, "/", length(out_list)))
            out_list[[the_index + 1]] <- m
            r <- try_extract_fit(m)
            out_df[i - 1, j + 1] <- r
            message(paste0("Processed model with n_profiles = ", i, " and model = ", j))
            if (is.numeric(r)) {
                message(paste0("Result: BIC = ", r))
            } else {
                message("Result: ", r)
            }
        }
    }

    print(out_df)

    if (!is.null(save_models)) {
        readr::write_rds(m, save_models)
        return(out_df)
    } else {
        invisible(list(out_df, m))
    }
}

extract_warnings <- function(x){
    x$warnings
}

extract_errors <- function(x){
    x$errors[[1]]
}

try_extract_fit <- function(m) {
    out <- tryCatch(
        {
            warnings_list <- extract_warnings(m)
            errors_list <- extract_errors(m)
            if (errors_list[[1]][1] == "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY DUE TO AN INSUFFICIENT" | errors_list[[1]][1] == "THE LOGLIKELIHOOD DECREASED IN THE LAST EM ITERATION.  CHANGE YOUR MODEL") {
                return("Convergence problem")
            } else if (warnings_list[[2]][1] == "WARNING:  THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED.  THE") {
                return("LL not replicated")
            }
        },
        error=function(cond) {
            return(m$summaries$BIC)
        }
    )
    return(out)
}
