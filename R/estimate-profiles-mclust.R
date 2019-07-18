#' Estimate latent profiles using mclust
#'
#' Estimates latent profiles (finite mixture models) using the open source
#' package \code{\link[mclust:Mclust]{mclust}}.
#' @param df data.frame with two or more columns with continuous variables
#' @param n_profiles Numeric vector. The number of profiles (or mixture
#' components) to be estimated. Each number in the vector corresponds to an
#' analysis with that many mixture components.
#' @param model_numbers Numeric vector. Numbers of the models to be estimated.
#' See \code{\link{estimate_profiles}} for a description of the models available
#' in tidyLPA.
#' @param select_vars Character. Optional vector of variable names in \code{df},
#' to be used for model estimation. Defaults to \code{NULL}, which means all
#' variables in \code{df} are used.
#' @param ... Parameters passed directly to \code{\link[mclust]{Mclust}}. See
#' the documentation of \code{\link[mclust]{Mclust}}.
#' @author Caspar J. van Lissa
#' @return An object of class 'tidyLPA' and 'list'
estimate_profiles_mclust <- function(df, n_profiles, model_numbers, select_vars, ...){
    df_full <- df
    df <- df[, select_vars, drop = FALSE]
    arg_list <- match.call()
    warnings <- NULL
    no_na_rows <- !apply(df, 1, anyNA)
    if(any(!no_na_rows)){
        warning("The mclust algorithm does not allow for missing data. Some rows were omitted from analysis. Consider using Mplus, which accounts for cases with partially missing data, or use a non-parametric single imputation technique prior to analysis, such as the R-package 'missForest'.\n")
    }
    full_data <- df[no_na_rows, , drop = FALSE]
    boot_model_names <- get_modelname(model_numbers)
    if(ncol(full_data) == 1){
        boot_model_names <- substr(boot_model_names, 1, 1)
    }
    run_models <- expand.grid(prof = n_profiles, mod = model_numbers)


    boot_blrt <- lapply(boot_model_names, function(mod_name){
        mclustBootstrapLRT(full_data,
                           modelName = mod_name,
                           nboot = ifelse(methods::hasArg("nboot"), arg_list$nboot, 100),
                           maxG = max(n_profiles),
                           verbose = FALSE)
    })


    out_list <- mapply(FUN = function(this_class, this_model){
        out <- list(model = Mclust(full_data,
               G = this_class,
               modelNames = ifelse(ncol(full_data) == 1, substr(get_modelname(this_model), 1, 1), get_modelname(this_model)),
               warn = FALSE,
               verbose = FALSE,
               ...
        ))
        if(is.null(out$model)){
            warning("Mclust could not estimate model ", this_model, " with ", this_class, " classes.", call. = FALSE)
            out$model$mclustBootstrap <- out$model$LRTS <- out$model$LRTS_p <- out$estimates <- out$dff <- NULL
            out$fit <- c(Model = this_model, Classes = this_class, rep(NA, 16))
            warnings <- c(warnings, paste0("Mclust could not estimate model ", this_model, " with ", this_class, " classes."))
        } else {
            out$model$mclustBootstrap <- MclustBootstrap(out$model, nboot = 100, type = "bs", verbose = FALSE)
            out$model$LRTS <- ifelse(this_class == 1, NA, boot_blrt[[which(model_numbers == this_model)]]$obs[this_class-1])
            out$model$LRTS_p <- ifelse(this_class == 1, NA, boot_blrt[[which(model_numbers == this_model)]]$p.value[this_class-1])
            out$fit <- c(Model = this_model, Classes = this_class, calc_fitindices(out$model))
            estimates <- estimates(out$model)
            estimates$Model <- this_model
            estimates$Classes <- this_class
            if(this_class == 1){
                estimates$se[estimates$Category == "Means"] <- estimates$Estimate[estimates$Category == "Variances"]/out$model$n
                estimates$se[estimates$Category == "Variances"] <- sapply(sqrt(estimates$Estimate[estimates$Category == "Variances"]),
                                                                          se_s, n = out$model$n)^2
                estimates$p <- stats::pnorm(abs(estimates$Estimate), sd = estimates$se, lower.tail = FALSE)
            }
            out$estimates <- estimates
            outdat <- cbind(out$model$z, out$model$classification)
            dff <- matrix(NA, dim(df)[1], dim(outdat)[2])
            dff[no_na_rows, ] <- outdat
            colnames(dff) <- c(paste0("CPROB", 1:ncol(out$model$z)), "Class")
            out$dff <- as_tibble(cbind(df_full, dff))
            out$dff$model_number <- this_model
            out$dff$classes_number <- this_class
            out$dff <- out$dff[, c((ncol(out$dff)-1), ncol(out$dff), 1:(ncol(out$dff)-2))]
            attr(out$dff, "selected") <- names(df)
            # Set warnings
            if(out$fit[["prob_min"]]< .001) warnings <- c(warnings, "Some classes were not assigned any cases with more than .1% probability. Consequently, these solutions are effectively identical to a solution with one class less.")
            if(out$fit[["n_min"]] < .01) warnings <- c(warnings, "Less than 1% of cases were assigned to one of the profiles. Interpret this solution with caution and consider other models.")

        }
        out$warnings <- warnings
        class(out) <- c("tidyProfile.mclust", "tidyProfile", "list")
        out
    }, this_class = run_models$prof, this_model = run_models$mod, SIMPLIFY = FALSE)

    names(out_list) <- paste("model_", run_models$mod, "_class_", run_models$prof, sep = "")
    out_list
}
