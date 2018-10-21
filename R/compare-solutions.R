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
    check_too_few(fits)
    if(nrow(fits) == 1) stop("In order to compare_solutions, the tidyLPA object must contain more than one model.")
    fit_indices <- c("LogLik" = 1, "AIC" = -1, "AWE" = -1, "BIC" = -1, "CAIC" = -1, "CLC" = -1, "KIC" = -1, "SABIC" = -1, "ICL" = 1)
    max_these <- matrix(rep(fit_indices, nrow(fits)), nrow = nrow(fits), byrow = TRUE)
    best_model <- apply(max_these * fits[, names(fit_indices)], 2, which.max)
    AHP_best <- AHP(fits)
    p <- plot.tidyLPA(x, statistic = statistics)
    print(p)
    out <- list(fits = fits, best = best_model, AHP = AHP_best, plot = p, statistics = statistics)
    class(out) <- c("bestLPA", class(out))
    out
}

#' @title Print bestLPA
#' @description S3 method 'print' for class 'bestLPA'.
#' @param x An object of class 'bestLPA'.
#' @param digits minimal number of significant digits, see
#' \code{\link[base]{print.default}}.
#' @param na.print a character string which is used to indicate NA values in
#' printed output, or NULL. See \code{\link[base]{print.default}}.
#' @param ... further arguments to be passed to or from other methods. They are
#' ignored in this function.
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' if(interactive()){
#' results <- iris %>%
#'   select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'   estimate_profiles(1:3) %>%
#'   compare_solutions()
#' print(results)
#' }
#' }
#' @export
print.bestLPA <- function(x, digits = getOption("digits"), na.print = "", ...){
    cat("Compare tidyLPA solutions:\n\n")
    stats <- x$statistics
    dat <- as.matrix(x$fits[, c("Model", "Classes", stats)])
    miss_val <- is.na(dat)
    #dat$Model <- paste("Model ", dat$Model)
    #sprintf("%-9s", paste0(names(x$fitindices), ":")),
    dat[,3:ncol(dat)] <- sapply(dat[,3:ncol(dat)], formatC, digits = digits, format = "f")
    dat[miss_val] <- ""
    #rownames(dat) <- ""
    prmatrix(dat, rowlab = rep("", nrow(dat)), quote = FALSE, na = na.print)

    for(ft in x$statistics){
        #ft <- x$statistics[1]
        cat("\nBest model according to", ft, "is Model", dat[, "Model"][x$best[ft]], "with", dat[, "Classes"][x$best[ft]], "classes.")
    }
    cat("\n\nAn analytic hierarchy process, based on the fit indices AIC, AWE, BIC, CLC, and KIC (Akogul & Erisoglu, 2017), suggests the best solution is Model", dat[, "Model"][x$AHP], "with", dat[, "Classes"][x$AHP], "classes.")
}

