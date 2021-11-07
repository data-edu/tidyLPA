#' Estimate latent profiles using OpenMx
#'
#' Estimates latent profiles (finite mixture models) using the R-package OpenMx.
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
#' @param ... Parameters passed to and from functions.
#' @author Caspar J. van Lissa
#' @return An object of class 'tidyLPA' and 'list'
#' @importFrom methods hasArg
#' @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
#' @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
#' @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
#' @importFrom lavaan mplus2lavaan.modelSyntax
#' @importFrom stats cutree dist hclust
#' @importFrom utils capture.output
# @import OpenMx
estimate_profiles_openmx <-
    function(df, n_profiles, model_numbers, select_vars, ...) {
        arg_list <- as.list(match.call())[-1]
        df_full <- df
        df <- df[, select_vars, drop = FALSE]
        all_na_rows <- rowSums(is.na(df)) == ncol(df)
        if(any(all_na_rows)){
            warning("Data set contains cases with missing on all variables. These cases were not included in the analysis.\n")
            df <- df[!all_na_rows, , drop = FALSE]
        }

        param_names <- selected_variables <- names(df)

        # Check which arguments the user tried to pass.
        # First, check for arguments that cannot be used at all


        # Next, check for arguments that are constructed by estimate_profiles
        Args <- list(...)
        if(any(c("rdata", "usevariables", "MODEL") %in% names(Args))){
            illegal_args <- c("rdata", "usevariables", "MODEL")[which(c("rdata", "usevariables", "MODEL") %in% names(Args))]
            stop("The following illegal arguments were detected in the call to estimate_profiles:\n", paste0("  ", illegal_args, collapse = "\n"), "\nThese arguments are constructed by estimate_profiles(), and cannot be passed by users. Drop these arguments, and try again.", sep = "", call. = FALSE)
        }

        run_models <-
            expand.grid(prof = n_profiles, mod = model_numbers)
        # Create outlist
        out_list <- mapply(
            FUN = function(this_class, this_model) {
                warns <- NULL

                # Run analysis ------------------------------------------------------------
                suppressMessages(invisible({
                    mix_fit <- tryCatch({
                        Args_mx <- c(list(
                            data = df,
                            classes = this_class),
                            number_to_varcov(this_model),
                            Args)
                        do.call(tidySEM::mx_profiles, Args_mx)
                    },
                    error = function(e){
                        warns <<- c(warns, "Model did not converge.")
                        NULL
                    },
                    warning = function(w){
                        warns <<- c(warns, w)
                        NULL
                    })
                   }))
                out <- list(model = mix_fit)
                if(isTRUE(mix_fit$output$status$code == 0)){
                    out$fit <-
                        cbind(Model = this_model,
                          Classes = this_class,
                          calc_fitindices(mix_fit))
                    estimates <- estimates(out$model)
                    if(!is.null(estimates)){
                        estimates$Model <- this_model
                        estimates$Classes <- this_class
                    }
                    out$estimates <- estimates

                    outdat <- class_prob(mix_fit)$individual
                    outdat <- cbind(outdat, apply(outdat, 1, which.max))
                    dff <- matrix(NA_real_, dim(df_full)[1], dim(outdat)[2])
                    dff[!all_na_rows, ] <- outdat
                    colnames(dff) <- c(paste0("CPROB", 1:(ncol(dff)-1)), "Class")
                    out$dff <- as_tibble(cbind(df_full, dff))
                    out$dff$model_number <- this_model
                    out$dff$classes_number <- this_class
                    out$dff <- out$dff[, c((ncol(out$dff)-1), ncol(out$dff), 1:(ncol(out$dff)-2))]
                    attr(out$dff, "selected") <- selected_variables

                    # Check for warnings ------------------------------------------------------

                    if(!is.na(out$fit[["prob_min"]])){
                        if(out$fit[["prob_min"]]< .001) warns <- c(warns, "Some classes were not assigned any cases with more than .1% probability. Consequently, these solutions are effectively identical to a solution with one class less.")
                    }
                    if(!is.na(out$fit[["n_min"]])){
                        if(out$fit[["n_min"]] < .01) warns <- c(warns, "Less than 1% of cases were assigned to one of the profiles. Interpret this solution with caution and consider other models.")
                    }
                } else {
                    out$fit <-
                        c(Model = this_model,
                          Classes = this_class,
                          "LogLik" = NA, "AIC" = NA, "AWE" = NA, "BIC" = NA, "CAIC" = NA, "CLC" = NA, "KIC" = NA, "SABIC" = NA, "ICL" = NA, "Entropy" = NA, "prob_min" = NA, "prob_max" = NA, "n_min" = NA, "n_max" = NA, "BLRT_val" = NA, "BLRT_p" = NA)
                    out$estimates <- NULL
                }
                if(length(warns) > 0) out$warnings <- warns
                class(out) <- c("tidyProfile.OpenMx", "tidyProfile", "list")
                out
            },
            this_class = run_models$prof,
            this_model = run_models$mod,
            SIMPLIFY = FALSE
        )
        names(out_list) <-
            paste("model_", run_models$mod, "_class_", run_models$prof, sep = "")
        # Arguments for mxCompare ----------------------------------------
        if(length(out_list) > 1){
            comparethese <- rep(TRUE, length(out_list)-1)
            nclasses <- sapply(out_list, function(i)tryCatch({i$fit$Classes}, error = function(e){NA}))
            nclasses <- diff(nclasses)
            comparethese[which(nclasses < 0 | is.na(nclasses))] <- FALSE
            complexity <- sapply(out_list, function(i)tryCatch({i$fit$parameters}, error = function(e){NA}))
            complexity <- diff(complexity)
            comparethese[which(complexity < 0 | is.na(complexity))] <- FALSE
            if(any(comparethese)){
                comparisons <- cbind(1:(length(out_list)-1), 2:length(out_list))
                comparisons <- comparisons[comparethese, , drop = FALSE]
                dots <- list(...)
                Args_boot <- dots[which(names(dots) %in% c("replications", "previousRun", "checkHess"))]
                if(is.null(Args_boot[["replications"]])) Args_boot[["replications"]] <- 100
                Args_boot$boot <- TRUE
                for(this_comp in 1:nrow(comparisons)){
                    Args_boot[["base"]] <- out_list[[comparisons[this_comp, 2]]][["model"]]
                    Args_boot[["comparison"]] <- out_list[[comparisons[this_comp, 1]]][["model"]]
                    warnopt <- getOption("warn")
                    options(warn = 1)
                    theoutput <- capture.output({out <- tryCatch({do.call(mxCompare, Args_boot)}, error = function(e){NULL})}, type = "message")
                    options(warn = warnopt)
                    if(is.null(out)){
                        out_list[[comparisons[this_comp, 2]]][["warns"]] <- c(out_list[[comparisons[this_comp, 2]]][["warns"]], "Unable to perform BLRT.")
                    } else {
                        out_list[[comparisons[this_comp, 2]]]$fit["BLRT_val"] <- out$diffLL[2]
                        out_list[[comparisons[this_comp, 2]]]$fit["BLRT_p"] <- out$p[2]
                    }
                    if(length(theoutput) > 0){
                        out_list[[comparisons[this_comp, 2]]][["warnings"]] <- c(out_list[[comparisons[this_comp, 2]]][["warnings"]], theoutput)
                    }
                }

            }
        }
        out_list
    }
