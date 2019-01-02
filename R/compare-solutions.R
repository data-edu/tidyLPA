#' Compare latent profile models
#'
#' Takes an object of class 'tidyLPA', containing multiple latent profile models
#' with different number of classes or model specifications, and helps select
#' the optimal number of classes and model specification.
#' @param x An object of class 'tidyLPA'.
#' @param statistics Character vector. Which statistics to examine for
#' determining the optimal model. Defaults to 'BIC'.
#' @return An object of class 'bestLPA' and 'list', containing a tibble of fits
#' 'fits', a named vector 'best', indicating which model fit best according to
#' each fit index, a numeric vector 'AHP' indicating the best model according to
#' the \code{\link{AHP}}, an object 'plot' of class 'ggplot', and a numeric
#' vector 'statistics' corresponding to argument of the same name.
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' results <- iris %>%
#'   select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'   estimate_profiles(1:3) %>%
#'   compare_solutions()
#' }
#' @export
compare_solutions <- function(x, statistics = "BIC") {
    deprecated_arguments(c(
        "n_profiles_range" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "return_table" = "compare_solutions returns both a table ($fits) and a plot ($plot).",
        "models" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "model" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "toreturn" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "return_posterior_probs" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "return_original_df" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "prior_control" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "print_which_stats" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "center_raw_data" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation).",
        "scale_raw_data" = "compare_solutions no longer conducts the analysis; it merely compares the results of an analysis conducted using 'estimate_profiles' (run: ?estimate_profiles for documentation)."
    ))

    fits <- get_fit(x)
    if(nrow(fits) == 1) stop("In order to compare_solutions, the tidyLPA object must contain more than one model.")
    fit_indices <- c("LogLik" = 1, "AIC" = -1, "AWE" = -1, "BIC" = -1, "CAIC" = -1, "CLC" = -1, "KIC" = -1, "SABIC" = -1, "ICL" = 1)
    max_these <- matrix(rep(fit_indices, nrow(fits)), nrow = nrow(fits), byrow = TRUE)
    best_model <- apply(max_these * fits[, names(fit_indices)], 2, which.max)
    AHP_best <- AHP(fits)


# Check fits for problems -------------------------------------------------

<<<<<<< HEAD
  model <- case_when(
    variances == "equal" & covariances == "zero" ~ "EEI",
    variances == "varying" & covariances == "zero" ~ "VVI",
    variances == "equal" & covariances == "equal" ~ "EEE",
    # variances == "varying" & covariances == "equal" ~ 4, # I'd remove
    # variances == "equal" & covariances == "varying" ~ 5,
    variances == "varying" & covariances == "varying" ~ "VVV"
  )

  titles <- c(
    "Equal variances and covariances fixed to zero (model 1)",
    "Varying variances and covariances fixed to zero (model 2)",
    "Equal variances and equal covariances (model 3)",
    # "Varying variances and equal covariances (model 4)",
    # "Equal variances and varying covariances (model 5)",
    "Varying variances and varying covariances (model 6)"
  )

  titles_df <- data.frame(Model = model, titles)

  if (prior_control == FALSE) {
    if (statistic == "BIC") {
      x <- suppressWarnings(mclustBIC(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE
      ))
    } else if (statistic == "ICL") {
      x <- suppressWarnings(mclustICL(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE
      ))
=======
    warnings <- lapply(x, `[[`, "warnings")
    low_prob <- fits$prob_min < .001
    best_model <- apply(max_these * fits[, names(fit_indices)], 2, which.max)
    if(any(low_prob)){
        warnings[low_prob] <- lapply(warnings[low_prob], c, "Some classes were not assigned any cases with more than .1% probability. Consequently, these solutions are effectively identical to a solution with one class less.")
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc
    } else {
        too_few <- fits$n_min < .01
        if(any(too_few)){
            warnings[too_few] <- lapply(warnings[too_few], c, "Less than 1% of cases were assigned to one of the profiles. Interpret this solution with caution and consider other models.")
        }
    }

<<<<<<< HEAD
  to_plot <- y %>%
    gather("Model", "val", -.data$n_profiles) %>%
    mutate(
      "Model" = as.factor(.data$`Model`),
      val = abs(.data$val)
    ) %>%
    left_join(titles_df, by = "Model")
=======
    if(length(unique(fits$Classes)) > 1){
        if(any(best_model == min(fits$Classes))){
            warnings[which.min(fits$Classes)] <- lapply(warnings[which.min(fits$Classes)], c, "This solution has the minimum number of classes under consideration, and was considered to be the best solution according to one or more fit indices. Examine your results with care; mixture modeling might be unnecessary.")
        }
        if(any(best_model == max(fits$Classes))){
            warnings[which.max(fits$Classes)] <- lapply(warnings[which.max(fits$Classes)], c, "This solution has the maximum number of classes under consideration, and was considered to be the best solution according to one or more fit indices. Examine your results with care and consider estimating more classes.")
        }
    }
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc

    fits$Warnings <- ifelse(sapply(warnings, is.null), NA, "Warnings")
    if(any(!is.na(fits$Warnings))){
        warning("One or more analyses resulted in warnings! Examine these analyses carefully.")
    }

    out <- list(fits = fits, best = best_model, AHP = AHP_best, statistics = statistics, warnings = warnings)
    class(out) <- c("bestLPA", class(out))
    out
}

<<<<<<< HEAD
  p <- ggplot(to_plot, aes_string(
    x = "n_profiles",
    y = "val",
    color = "titles",
    group = "titles"
  )) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ylab(paste0(statistic, " (smaller value is better)")) +
    theme_bw() +
    ggplot2::scale_color_discrete("") +
    xlab("Profiles")
=======
#' @method print bestLPA
#' @export
print.bestLPA <- function(x, digits = 3, na.print = "", ...){
    cat("Compare tidyLPA solutions:\n\n")
    stats <- x$statistics
    dat <- as.matrix(x$fits[, c("Model", "Classes", stats)])
    miss_val <- is.na(dat)
    dat[,3:ncol(dat)] <- sapply(dat[,3:ncol(dat)], formatC, digits = digits, format = "f")
    dat[miss_val] <- ""
    #rownames(dat) <- ""
    prmatrix(dat, rowlab = rep("", nrow(dat)), quote = FALSE, na.print = na.print)
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc

    for(ft in x$statistics){
        #ft <- x$statistics[1]
        cat("\nBest model according to", ft, "is Model", dat[, "Model"][x$best[ft]], "with", dat[, "Classes"][x$best[ft]], "classes.")
    }
    cat("\n\nAn analytic hierarchy process, based on the fit indices AIC, AWE, BIC, CLC, and KIC (Akogul & Erisoglu, 2017), suggests the best solution is Model", dat[, "Model"][x$AHP], "with", dat[, "Classes"][x$AHP], "classes.")
}

