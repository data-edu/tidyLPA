#' Explore BIC for various MPlus model solutions
#' @details Explore the BIC values of a range of Mplus models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_max a vector with the range of the number of mixture components to explore; defaults to 2 through 10 (2:10)
#' @param model which models to include; defaults to 1:6 (see https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html)
#' @param save_models whether to save the models as an rds file (i.e., set to "output.rds" to save the models with this filename)
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to FALSE
#' @inheritParams estimate_profiles_mplus
#' @return a list with a data.frame with the BIC values and a list with all of the model output; if save_models is the name of an rds file (i.e., "out.rds"), then the model output will be written with that filename and only the data.frame will be returned
#' @import mclust
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' compare_solutions_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#' n_profiles_max = 4)
#' }
#' @export

compare_solutions_mplus <- function(df, ...,
                                    n_profiles_max = 10,
                                    model = 1:6,
                                    starts = c(20, 4),
                                    m_iterations = 500,
                                    st_iterations = 10,
                                    convergence_criterion = 1E-6,
                                    save_models = NULL,
                                    return_table = FALSE) {
  message("Note that this (and other functions that use MPlus) is at the experimental stage! Please provide feedback at https://github.com/jrosen48/tidyLPA")
  out_df <- data.frame(matrix(ncol = length(model), nrow = (n_profiles_max - 1)))
  names(out_df) <- paste0("model_", model)
  out_df <- out_df %>%
    mutate(n_profiles = 2:n_profiles_max) %>%
    select(.data$n_profiles, everything())

  out_list <- as.list(rep(NA, times = (nrow(out_df) * ncol(out_df))))

  for (i in 2:n_profiles_max) {
    for (j in model) {
      m <- estimate_profiles_mplus(
        df, ...,
        n_profiles = i,
        model = j,
        starts = starts,
        m_iterations = m_iterations,
        convergence_criterion = convergence_criterion,
        st_iterations = st_iterations,
        return_save_data = F
      )
      the_index <- sum(!is.na(out_list))
      message(paste0("Model ", the_index + 1, "/", length(out_list)))
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
  }

  if (return_table == TRUE) {
    return(out_df)
  } else {
    p <- out_df %>%
      tidyr::gather("key", "val", -.data$n_profiles) %>%
      dplyr::filter(
        .data$val != "Convergence problem",
        .data$val != "LL not replicated"
      ) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "n_profiles", y = "val", shape = "key", color = "key", group = "key")) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
    print(p)
    return(p)
  }
}

extract_warnings <- function(x) {
  x$warnings
}

extract_errors <- function(x) {
  x$errors[[1]]
}

try_extract_fit <- function(m) {
  out <- tryCatch(
    {
      warnings_list <- extract_warnings(m)
      errors_list <- extract_errors(m)
      if (stringr::str_detect(errors_list[[1]][1], "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") |
        stringr::str_detect(errors_list[[1]][1], "THE LOGLIKELIHOOD DECREASED IN THE LAST EM ITERATION.")) {
        return("Convergence problem")
      } else if (warnings_list[[2]][1] == "WARNING:  THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED.  THE") {
        return("LL not replicated")
      }
    },
    error = function(cond) {
      return(m$summaries$BIC)
    }
  )
  return(out)
}
