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
#' iris_subset <- sample(nrow(iris), 20) # so examples execute quickly
#' results <- iris %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width",
#'     "Petal.Length", "Petal.Width")) %>%
#'   estimate_profiles(1:3) %>%
#'   compare_solutions()
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
    error_models <- is.na(fits$LogLik)
    if(sum(!error_models) < 2) stop("In order to compare_solutions, the tidyLPA object must contain more than one model.")
    fit_indices <- c("LogLik" = 1, "AIC" = -1, "AWE" = -1, "BIC" = -1, "CAIC" = -1, "CLC" = -1, "KIC" = -1, "SABIC" = -1, "ICL" = 1)
    max_these <- matrix(rep(fit_indices, nrow(fits)), nrow = nrow(fits), byrow = TRUE)
    best_model <- apply(max_these * fits[, names(fit_indices)], 2, which.max)

    AHP_best <- c(1:nrow(fits))[!error_models][AHP(fits[!error_models, ])]

# Check fits for problems -------------------------------------------------

    warnings <- sapply(x, function(x){!is.null(x[["warnings"]])})
    if(any(warnings)){
        fits$Warnings <- ifelse(warnings, "Warning", NA)
        warning("\nOne or more analyses resulted in warnings! Examine these analyses carefully: ",
                paste(names(x)[which(warnings)], collapse = ", "),
                call. = FALSE)
    }

    best_model <- apply(max_these * fits[, names(fit_indices)], 2, which.max)

# Check for problems with comparing solutions -----------------------------

    if(length(unique(fits$Classes)) > 1){
        if(any(best_model == min(fits$Classes))){
            warning("The solution with the minimum number of classes under consideration was considered to be the best solution according to one or more fit indices. Examine your results with care", ifelse(min(fits$Classes == 1), "; mixture modeling might be unnecessary.", "; consider adding a smaller number of classes."), call. = FALSE)
        }
        if(any(best_model == max(fits$Classes))){
            warning("The solution with the maximum number of classes under consideration was considered to be the best solution according to one or more fit indices. Examine your results with care and consider estimating more classes.", call. = FALSE)
        }
    }

    out <- list(fits = fits, best = best_model, AHP = AHP_best, statistics = statistics)
    class(out) <- c("bestLPA", class(out))
    out
}

#' @method print bestLPA
#' @export
print.bestLPA <- function(x, digits = 3, na.print = "", ...){
    cat("Compare tidyLPA solutions:\n\n")
    stats <- x$statistics
    if("Warnings" %in% names(x$fits)){
        fit_cols <- c("Model", "Classes", stats, "Warnings")
    } else {
        fit_cols <- c("Model", "Classes", stats)
    }
    dat <- as.matrix(x$fits[, fit_cols])
    miss_val <- is.na(dat)
    dat[,3:ncol(dat)] <- sapply(dat[,3:ncol(dat)], formatC, digits = digits, format = "f")
    dat[miss_val] <- ""
    #rownames(dat) <- ""
    prmatrix(dat, rowlab = rep("", nrow(dat)), quote = FALSE, na.print = na.print)

    for(ft in x$statistics){
        #ft <- x$statistics[1]
        cat("\nBest model according to", ft, "is Model", dat[, "Model"][x$best[ft]], "with", dat[, "Classes"][x$best[ft]], "classes.")
    }
    cat("\n\nAn analytic hierarchy process, based on the fit indices AIC, AWE, BIC, CLC, and KIC (Akogul & Erisoglu, 2017), suggests the best solution is Model", dat[, "Model"][x$AHP], "with", dat[, "Classes"][x$AHP], "classes.\n")
}

