#' @title Estimate latent profiles
#' @description Estimates latent profiles (finite mixture models) using the open
#' source package \code{\link[mclust:Mclust]{mclust}}, or the commercial program
#' Mplus (using the R-interface of
#' \code{\link[MplusAutomation:mplusModeler]{MplusAutomation}}).
#' @param df data.frame of numeric data; continuous indicators are required for
#' mixture modeling.
#' @param n_profiles Integer vector of the number of profiles (or mixture
#' components) to be estimated.
#' @param models Integer vector. Set to \code{NULL} by default, and models are
#' constructed from the \code{variances} and \code{covariances} arguments. See
#' \code{Details} for the six models available in tidyLPA.
#' @param variances Character vector. Specifies which variance components to
#' estimate. Defaults to "equal" (constrain variances across profiles); the
#' other option is "varying" (estimate variances freely across profiles). Each
#' element of this vector refers to one of the models you wish to run.
#' @param covariances Character vector. Specifies which covariance components to
#' estimate. Defaults to "zero" (do not estimate covariances; this corresponds
#' to an assumption of conditional independence of the indicators); other
#' options are "equal" (estimate covariances between items, constrained across
#' profiles), and "varying" (free covariances across profiles).
#' @param package Character. Which package to use; 'mclust' or
#' 'MplusAutomation' (requires Mplus to be installed). Default: 'mclust'.
#' @param select_vars Character. Optional vector of variable names in \code{df},
#' to be used for model estimation. Defaults to \code{NULL}, which means all
#' variables in \code{df} are used.
#' @param ... Additional arguments are passed to the estimating function; i.e.,
#' \code{\link[mclust]{Mclust}}, or \code{\link[MplusAutomation]{mplusModeler}}.
#' @return A list of class 'tidyLPA'.
#' @details Six models are currently available in tidyLPA, corresponding to the
#' most common requirements. These are:
#' \enumerate{
#' \item Equal variances and covariances fixed to 0
#' \item Varying variances and covariances fixed to 0
#' \item Equal variances and equal covariances
#' \item Varying variances and equal covariances (not able to be fit w/ mclust)
#' \item Equal variances and varying covariances (not able to be fit w/ mclust)
#' \item Varying variances and varying covariances
#' }
#'
#' Two interfaces are available to estimate these models; specify their numbers
#' in the \code{models} argument (e.g., \code{models = 1}, or
#' \code{models = c(1, 2, 3)}), or specify the variances/covariances to be
#' estimated (e.g.,: \code{variances = c("equal", "varying"), covariances =
#' c("zero", "equal")}). Note that when mclust is used, \code{models =
#' c(1, 2, 3, 6)} are the only models available.
#' @examples
#'
#' iris_sample <- iris[c(1:4, 51:54, 101:104), ] # to make example run more quickly
#'
#' # Example 1:
#' iris_sample %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width",
#'     "Petal.Length")) %>%
#'   estimate_profiles(3)
#'
#' \donttest{
#' # Example 2:
#' iris %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width",
#'     "Petal.Length")) %>%
#'   estimate_profiles(n_profiles = 1:4, models = 1:3)
#'
#' # Example 3:
#' iris_sample %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width",
#'     "Petal.Length")) %>%
#'   estimate_profiles(n_profiles = 1:4, variances = c("equal", "varying"),
#'                     covariances = c("zero", "zero"))
#' }
#'
#' @export
estimate_profiles <- function(df,
                              n_profiles,
                              models = NULL,
                              variances = "equal",
                              covariances = "zero",
                              package = "mclust",
                              select_vars = NULL,
                              ...) {
    UseMethod("estimate_profiles", df)
}

#' @export
estimate_profiles.data.frame <- function(df,
                                         n_profiles,
                                         models = NULL,
                                         variances = "equal",
                                         covariances = "zero",
                                         package = "mclust",
                                         ...) {
    NextMethod("estimate_profiles", df)
}

#' @export
estimate_profiles.matrix <- function(df,
                                      n_profiles,
                                      models = NULL,
                                      variances = "equal",
                                      covariances = "zero",
                                      package = "mclust",
                                      ...) {
    df <- data.frame(df)
    NextMethod("estimate_profiles", df)
}

#' @export
estimate_profiles.numeric <- function(df,
                                                                  n_profiles,
                                                                  models = NULL,
                                                                  variances = "equal",
                                                                  covariances = "zero",
                                                                  package = "mclust",
                                                                  ...) {
    df <- data.frame(df)
    NextMethod("estimate_profiles", df)
}

#' @export
estimate_profiles.default <- function(df,
                                      n_profiles,
                                      models = NULL,
                                      variances = "equal",
                                      covariances = "zero",
                                      package = "mclust",
                                      select_vars = NULL,
                                      ...) {
    # Check deprecated arguments ----------------------------------------------

    deprecated_arguments(c(
        "model" = "Instead, use the argument 'models', or 'variances' and 'covariances'.",
        "toreturn" = "A tibble is returned by default in the $df element of the returned output.",
        "return_posterior_probs" = "Posterior probabilities are returned by default because they are important for evaluating number of classes and interpreting the solution.",
        "return_original_df" = "A df is returned by default because it is useful for plotting.",
        "prior_control" = "You can pass it directly to mclust() through the '...' argument.",
        "print_which_stats" = "We now return the statistics considered to be best-practice for determining the number of classes.",
        "center_raw_data" = "Before running estimate_profiles, run the function 'scale' on your data (see ?scale). Also look at the function 'poms' (percentage-of-maximum scoring) for a way to put your variables on an easily comparable scale.",
        "scale_raw_data" = "Before running estimate_profiles, run the function 'scale' on your data (see ?scale). Also see ?poms (percentage-of-maximum scoring) for a way to put your variables on an easily comparable scale."
    ))

    # Check if there is an argument 'select_vars'' ----------------------------
    if(!is.null(select_vars)){
        if(!all(select_vars %in% names(df))){
            stop("The following variables are not in df: ", paste(select_vars[!select_vars %in% names(df)], collapse = ", "))
        }
    } else {
        select_vars <- names(df)
    }
    # Backup df
    df_full <- df
    df <- df[, select_vars, drop = FALSE]

    # Deprecated variable selection -------------------------------------------
    check_dots <- match.call(expand.dots = FALSE)$`...`
    if(any(names(df) %in% unlist(check_dots))){
        warning("It looks like you are trying to extract some variables from df. This functionality is deprecated. Instead, estimate_profiles() always uses all variables in df. Select your variables prior to analysis, using either:\n  The dplyr function select(df, Your, Selected, Variable, Names), or\n  The base R function df[, c('Your', 'Selected', 'Variable', 'Names')]")
    }

    # Screen for legal input --------------------------------------------------
    package <- package[1]
    if(!inherits(package, "character")) stop("Argument package must be a character string.")
    package <- c("mclust", "mplusautomation")[pmatch(tolower(package), c("mclust", "mplusautomation"))]
    if(is.na(package)) stop("Argument 'package' did not match a valid package. Use either 'mclust' or 'MplusAutomation'.")

    # Screen df ---------------------------------------------------------------

    var_types <- sapply(df, class)
    if(any(!var_types %in% c("numeric", "integer"))){
        stop("estimate_profiles can only handle numeric variables. The variables violating this restriction are: ", paste(names(df)[!var_types %in% c("numeric", "integer")], collapse = ", "))
    }

    # Get model numbers --------------------------------------------------------
    if (all(sapply(c(models, variances, covariances), is.null))) {
        stop(
            "Please specify either the 'models' argument, or the 'variances' and 'covariances' arguments."
        )
    }

    if (!is.null(models)) {
        message("The 'variances'/'covariances' arguments were ignored in favor of the 'models' argument.")
        model_numbers <- models
    } else {
        if (length(variances) != length(covariances)) {
            stop(
                "The 'variances' and 'covariances' arguments must be vectors of equal length. Together, they describe the models to be run."
            )
        }
        model_numbers <-
            unname(
                mapply(
                    FUN = get_model_number,
                    variances = variances,
                    covariances = covariances
                )
            )
    }

    # If data.frame has only one column, covariances must be zero
    if(ncol(df) == 1){
        if(any(model_numbers > 2)) warning("Argument 'df' has only one column, so covariances were set to zero.")
        model_numbers <- 2 - (model_numbers %% 2) # Set to 1 or 2
    }

    out <- switch(package,
           "mplusautomation" = estimate_profiles_mplus2(df_full, n_profiles, model_numbers, select_vars, ...),
           "mclust" = estimate_profiles_mclust(df_full, n_profiles, model_numbers, select_vars, ...))

    # Check warnings here
    warnings <- sapply(out, function(x){!is.null(x[["warnings"]])})
    if(any(warnings)){
        warning("\nOne or more analyses resulted in warnings! Examine these analyses carefully: ",
                paste(names(out)[warnings], collapse = ", "),
                call. = FALSE)
    }

    #if (is.null(m)) stop("Model could not be estimated.")
    class(out) <- c("tidyLPA", "list")
    out
}


#' @title Get estimates from objects generated by tidyLPA
#' @description Get estimates from objects generated by tidyLPA.
#' @param x An object generated by tidyLPA.
#' @param ... further arguments to be passed to or from other methods. They are
#' ignored in this function.
#' @return A tibble.
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' if(interactive()){
#'  results <- iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(3)
#'  get_estimates(results)
#'  get_estimates(results[[1]])
#'  }
#' }
#' @export
get_estimates <- function(x, ...) {
    UseMethod("get_estimates", x)
}

#' @describeIn get_estimates Get estimates for a latent profile analysis with
#' multiple numbers of classes and models, of class 'tidyLPA'.
#' @export
get_estimates.tidyLPA <- function(x, ...) {
    as_tibble(do.call(rbind, lapply(x, `[[`, "estimates")))
}

#' @describeIn get_estimates Get estimates for a single latent profile analysis
#' object, of class 'tidyProfile'.
#' @export
get_estimates.tidyProfile <- function(x, ...) {
    x$estimates
}

#' @title Get fit indices from objects generated by tidyLPA
#' @description Get fit indices from objects generated by tidyLPA.
#' @param x An object generated by tidyLPA.
#' @param ... further arguments to be passed to or from other methods. They are
#' ignored in this function.
#' @return A tibble. Learn more at https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html#getting-fit-statistics
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' if(interactive()){
#'  results <- iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(3)
#'  get_fit(results)
#'  get_fit(results[[1]])
#'  }
#' }
#' @export
get_fit <- function(x, ...) {
    UseMethod("get_fit", x)
}

#' @describeIn get_fit Get fit indices for a latent profile analysis with
#' multiple numbers of classes and models, of class 'tidyLPA'.
#' @export
get_fit.tidyLPA <- function(x, ...) {
    as_tibble(t(sapply(x, `[[`, "fit")))
}

#' @describeIn get_fit Get fit indices for a single latent profile analysis
#' object, of class 'tidyProfile'.
#' @export
get_fit.tidyProfile <- function(x, ...) {
    x$fit
}

#' @title Get data from objects generated by tidyLPA
#' @description Get data from objects generated by tidyLPA.
#' @param x An object generated by tidyLPA.
#' @param ... further arguments to be passed to or from other methods. They are
#' ignored in this function.
#' @return If one model is fit, the data is returned in wide format as a tibble.
#' If more than one model is fit, the data is returned in long form. See the
#' examples.
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(dplyr)
#'  # the data is returned in wide form
#'  results <- iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(3)
#'  get_data(results)
#'
#'  # note that if more than one model is fit, the data is returned in long form
#'  results1 <- iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(c(3, 4))
#'  get_data(results1)
#'  }
#' }
#' @export
get_data <- function(x, ...) {
    UseMethod("get_data", x)
}

#' @describeIn get_data Get data for a latent profile analysis with multiple
#' numbers of classes and models, of class 'tidyLPA'.
#' @export
get_data.tidyLPA <- function(x, ...) {
    cl <- match.call()
    if(length(x) == 1){
        cl$x <- x[[1]]
        cl[[1]] <- as.name("get_data")
        return(eval.parent(cl))
    }
    as_tibble(do.call(.get_long_data, as.list(cl[-1])))
}

#' @describeIn get_data Get data for a single latent profile analysis object,
#' of class 'tidyProfile'.
#' @export
get_data.tidyProfile <- function(x, ...) {
    if(!is.null(x[["dff"]])){
        x[["dff"]]
    } else {
        stop("This tidyProfile has no data attached.")
    }
}


# Internal ----------------------------------------------------------------

.get_long_data <- function(x, ...) {
    if(inherits(x, "tidyProfile")){
        x <- list(x)
    }
    out <- lapply(x, function(x) {
        if(!is.null(x[["dff"]])){
            dt <- data.frame(x[["dff"]])
            prob_names <- grep("^CPROB", names(dt), value = TRUE)
            if (length(prob_names) > 1) {
                reshape(
                    dt,
                    varying = c("Probability" = prob_names),
                    direction = "long",
                    v.names = "Probability",
                    timevar = "Class_prob",
                    sep = ""
                )
            } else {
                cbind(
                    dt[, 1:(ncol(dt) - 2)],
                    Class = dt$Class,
                    Class_prob = dt$Class,
                    Probability = dt$CPROB1,
                    id = 1:nrow(dt)
                )
            }
        }
    })
    do.call(rbind, out)
}

#' @title Print tidyLPA
#' @description S3 method 'print' for class 'tidyLPA'.
#' @param x An object of class 'tidyLPA'.
#' @param stats Character vector. Statistics to be printed. Default:
#' c("AIC", "BIC", "Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_p"
#' ).
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
#'  iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(3)
#'  }
#' }
#' @export
print.tidyLPA <-
    function(x,
             stats = c("AIC",
                       "BIC",
                       "Entropy",
                       "prob_min",
                       "prob_max",
                       "n_min",
                       "n_max",
                       "BLRT_p"),
             digits = 2,
             na.print = "",
             ...) {
        fits <- get_fit(x)
        if(all(is.na(fits[, -c(1,2)]))){
            stop("This tidyLPA analysis does not contain any valid results. Most likely, all models failed to converge.", call. = FALSE)
        }
        dat <- as.matrix(fits[, c("Model", "Classes", stats)])
        miss_val <- is.na(dat)
        #dat$Model <- paste("Model ", dat$Model)
        #sprintf("%-9s", paste0(names(x$fitindices), ":")),
        dat[, 3:ncol(dat)] <-
            sapply(dat[, 3:ncol(dat)], formatC, digits = digits, format = "f")
        dat[miss_val] <- ""
        #rownames(dat) <- ""
        cat("tidyLPA analysis using", paste0(gsub("^tidyProfile\\.", "", class(x[[1]])[1]), ":"), "\n\n")

        prmatrix(dat,
                 rowlab = rep("", nrow(dat)),
                 quote = FALSE,
                 na.print = na.print)
    }

#' @title Print tidyProfile
#' @description S3 method 'print' for class 'tidyProfile'.
#' @param x An object of class 'tidyProfile'.
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
#'  tmp <- iris %>%
#'    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'    estimate_profiles(3)
#'  tmp[[2]]
#'  }
#' }
#' @export
print.tidyProfile <-
    function(x,
             digits = 2,
             na.print = "",
             ...) {
        dat <- get_fit(x)
        miss_val <- is.na(dat)
        dat[3:length(dat)] <-
            formatC(dat[3:length(dat)], digits = digits, format = "f")
        dat[miss_val] <- ""

        cat("tidyProfile estimated using", paste0(gsub("^tidyProfile\\.", "", class(x[[1]])[1]), ":"), "\n\n")
        print(dat, quote = FALSE, na = na.print)

    }
