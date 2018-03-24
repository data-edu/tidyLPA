#' Explore BIC for various models and numbers of profiles using MPlus (requires purchasing and installing MPlus to use)
#' @details Explore the BIC values of a range of Mplus models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_min lower bound of the number of profiles to explore; defaults to 2
#' @param n_profiles_max upper bound of the number of profiles to explore; defaults to 10
#' @param model which models to include; defaults to 1:6 (see https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html)
#' @param save_models whether to save the models as rds files
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to FALSE
#' @param return_stats_df whether to return a list of fit statistics for the solutions explored; defaults to FALSE
#' @inheritParams estimate_profiles_mplus
#' @return a list with a data.frame with the BIC values and a list with all of the model output; if save_models is the name of an rds file (i.e., "out.rds"), then the model output will be written with that filename and only the data.frame will be returned
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' compare_solutions_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#' n_profiles_max = 4)
#' }
#' @export

compare_solutions_mplus <- function(df, ...,
                                    n_profiles_min = 2,
                                    n_profiles_max = 10,
                                    model = 1:6,
                                    starts = c(20, 4),
                                    m_iterations = 500,
                                    st_iterations = 10,
                                    convergence_criterion = 1E-6,
                                    save_models = FALSE,
                                    return_table = TRUE,
                                    n_processors = 1,
                                    return_stats_df = FALSE,
                                    include_LMR = TRUE,
                                    include_BLRT = FALSE) {
    message("Note that this and other functions that use MPlus are at the experimental stage! Please provide feedback at https://github.com/jrosen48/tidyLPA")

    out_df <- data.frame(matrix(ncol = length(model), nrow = (n_profiles_max - (n_profiles_min - 1))))
    names(out_df) <- paste0("model_", model)

    out_df <- out_df %>%
        mutate(n_profiles = n_profiles_min:n_profiles_max) %>%
        select(.data$n_profiles, dplyr::everything())

    counter <- 0

    if (save_models == TRUE) {
        if (!dir.exists("compare_solutions_lpa_output")) dir.create("compare_solutions_lpa_output")
    }

    if (save_models == TRUE) {
        remove_tmp_files <- FALSE
    } else {
        remove_tmp_files <- TRUE
    }

    for (i in n_profiles_min:n_profiles_max) {
        for (j in model) {
            message(paste0("Processing model with n_profiles = ", i, " and model = ", j))
            m <- suppressMessages(estimate_profiles_mplus(
                df, ...,
                n_profiles = i,
                model = j,
                starts = starts,
                m_iterations = m_iterations,
                convergence_criterion = convergence_criterion,
                st_iterations = st_iterations,
                return_save_data = F,
                n_processors = n_processors,
                include_LMR = include_LMR,
                include_BLRT = include_BLRT,
                remove_tmp_files = remove_tmp_files
            ))

            if (save_models == TRUE) {
                if (!dir.exists(stringr::str_c("compare_solutions_lpa_output/m", j, "_p", i))) dir.create(stringr::str_c("compare_solutions_lpa_output/m", j, "_p", i))
                capture <- utils::capture.output(m_all <- MplusAutomation::readModels("i.out")) # this will need to be changed to be dynamic
                readr::write_rds(m_all, stringr::str_c("compare_solutions_lpa_output/m", j, "_p", i, "/m", j, "_p", i, ".rds"))
            }

            counter <- counter + 1

            # fix (remove) suppressWarnings()
            if (m[1] == "Error: Convergence issue" | m[1] == "Warning: LL not replicated") {
                message(stringr::str_c("Result: ", m))
                out_df[i - (n_profiles_min - 1), j + 1] <- m
            } else {
                message(paste0("Result: BIC = ", m$summaries$BIC))
                out_df[i - (n_profiles_min - 1), j + 1] <- m$summaries$BIC

                if (!("T11_VLMR_2xLLDiff" %in% names(m$summaries))) {
                    VLMR_val <- NA
                    VLMR_p <- NA
                } else {
                    VLMR_val <- m$summaries$T11_VLMR_2xLLDif
                    VLMR_p <- m$summaries$T11_VLMR_PValue
                }

                if (!("BLRT_2xLLDiff" %in% names(m$summaries))) {
                    BLRT_val <- NA
                    BLRT_p <- NA
                } else {
                    BLRT_val <- m$summaries$BLRT_2xLLDiff
                    BLRT_p <- m$summaries$BLRT_PValue
                }

                if (counter == 1) {
                    stats_df <- data.frame(n_profile = i,
                                           model = j,
                                           LL = m$summaries$LL,
                                           AIC = m$summaries$LL,
                                           BIC = m$summaries$BIC,
                                           SABIC = m$summaries$aBIC,
                                           CAIC = m$summaries$AICC,
                                           Entropy = m$summaries$Entropy,
                                           VLMR_val = VLMR_val,
                                           VLMR_p = VLMR_p,
                                           LMR_val = m$summaries$T11_LMR_Value,
                                           LMR_p = m$summaries$T11_LMR_PValue,
                                           BLRT_val = BLRT_val,
                                           BLRT_p = BLRT_p)
                } else {

                    d <- data.frame(n_profile = i,
                                    model = j,
                                    LL = m$summaries$LL,
                                    AIC = m$summaries$LL,
                                    BIC = m$summaries$BIC,
                                    SABIC = m$summaries$aBIC,
                                    CAIC = m$summaries$AICC,
                                    Entropy = m$summaries$Entropy,
                                    VLMR_val = VLMR_val,
                                    VLMR_p = VLMR_p,
                                    LMR_val = m$summaries$T11_LMR_Value,
                                    LMR_p = m$summaries$T11_LMR_PValue,
                                    BLRT_val = BLRT_val,
                                    BLRT_p = BLRT_p)

                    stats_df <- dplyr::bind_rows(stats_df, d)
                }

            }
        }
    }

    if (return_stats_df == TRUE & return_table == TRUE) {
        return(list(dplyr::as_tibble(out_df), dplyr::arrange(dplyr::as_tibble(stats_df), .data$model, .data$n_profile)))
    }

    if (return_stats_df == TRUE) {
        print(dplyr::as_tibble(out_df))
        return(dplyr::arrange(dplyr::as_tibble(stats_df), .data$model, .data$n_profile))
    }

    if (return_table == TRUE) {
        print(dplyr::as_tibble(out_df))
        invisible(dplyr::as_tibble(out_df))
    } else {
        out_df %>%
            tidyr::gather("Model", "BIC", -.data$n_profiles) %>%
            dplyr::filter(stringr::str_detect(.data$BIC, "\\d+\\.*\\d*")) %>%
            dplyr::mutate(BIC = as.numeric(.data$BIC),
                          n_profiles = as.integer(.data$n_profiles),
                          Model = stringr::str_extract(.data$Model, "\\d")) %>% 
        ggplot2::ggplot(ggplot2::aes_string(x = "n_profiles", y = "BIC", shape = "Model", color = "Model", group = "Model")) +
            ggplot2::geom_line() +
            ggplot2::geom_point()
    }
}
