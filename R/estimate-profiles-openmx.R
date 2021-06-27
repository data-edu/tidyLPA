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
#' @importFrom OpenMx omxAssignFirstParameters
#' @importFrom lavaan mplus2lavaan.modelSyntax
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
        # Prepare initial clustering
        clusts <- hclust(dist(df))
        # Create outlist
        out_list <- mapply(
            FUN = function(this_class, this_model) {
                # Generate specific Mplus object for each individual model
                model_class_specific <-
                    gsub("  ",
                         "\n",
                         syntax_class_specific(this_model, param_names))
                model_class_specific <- mplus2lavaan.modelSyntax(model_class_specific)
                model_class_specific <- paste0(
                    paste0(param_names, "~ m", param_names, "{C}*1\n", collapse = ""),
                    model_class_specific)

                classes <- lapply(1:this_class, function(i){
                    mxModel(as_ram(gsub("{C}", i, model_class_specific, fixed = TRUE)),
                            # Do not yet give fit function here, because model is run as multigroup first
                            #mxFitFunctionML(vector=TRUE),
                            name = paste0("class", i))
                })

                splits <- cutree(tree = clusts, k = this_class)
                strts <- lapply(1:max(this_class), function(i){
                    # Re-enable autostart if there is a problem with convergence;
                    # however, this may introduce problems with > 1 start value for constrained
                    # parameters
                    #mxAutoStart(
                        mxModel(classes[[i]], mxData(df[splits == i, , drop = FALSE], type = "raw"),
                                mxFitFunctionML())
                    #, type = "ULS")
                })
                strts <- do.call(mxModel, c(list(model = "mg_starts", mxFitFunctionMultigroup(paste0("class", 1:this_class)), strts)))
                strts <- mxAutoStart(strts, type = "ULS")
                # Use omxAssignFirstParameters if multiple starting values
                #
                strts_success <- FALSE
                warns <- NULL
                tryCatch({
                    #strts <- mxRun(omxAssignFirstParameters(strts))
                    suppressMessages(invisible({
                        strts <- mxRun(strts, silent = TRUE)
                    }))
                    strts_success <- TRUE
                }, error = function(e){
                    tryCatch({
                        #strts <<- mxTryHard(omxAssignFirstParameters(strts), extraTries = 100)
                        suppressMessages(invisible({
                            strts <- mxAutoStart(strts, type = "DWLS")
                            strts <<- mxTryHard(strts, extraTries = 100,
                                                silent = TRUE,
                                                verbose = FALSE,
                                                bestInitsOutput = FALSE)
                        }))
                        strts_success <<- TRUE
                    }, error = function(e2){
                        warns <<- c(warns, "Could not derive suitable starting values.")
                        })
                    })
                # Insert start values into mixture model
                classes <- mapply(function(cls, strt){
                    cls$M$values <- strts[[paste0("class", strt)]]$M$values
                    cls$S$values <- strts[[paste0("class", strt)]]$S$values
                    mxModel(cls, mxFitFunctionML(vector=TRUE))
                }, cls = classes, strt = 1:this_class)
                # Prepare mixture model
                mix <- mxModel(
                    model = paste0("mix", this_class),
                    classes,
                    mxData(df, "raw"),
                    mxMatrix(values=1, nrow=1, ncol=this_class, free=c(FALSE,rep(TRUE, this_class-1)), name="weights"),
                    mxExpectationMixture(paste0("class", 1:this_class), scale="softmax"),
                    mxFitFunctionML())
                # Run analysis ------------------------------------------------------------
                suppressMessages(invisible({
                    mix_fit <- tryCatch({
                        mxTryHard(mix,
                                  extraTries = 100,
                                  intervals=TRUE,
                                  silent = TRUE,
                                  verbose = FALSE,
                                  bestInitsOutput = FALSE,
                                  exhaustive = TRUE)
                    },
                    error = function(e){
                        warns <<- c(warns, "Model did not converge.")
                        NULL
                    },
                    warning = function(w){
                        warns <<- c(warns, w)
                    })
                   }))

                out <- list(model = mix_fit)
                if(mix_fit$output$status$code == 0){
                    out$fit <-
                        c(Model = this_model,
                          Classes = this_class,
                          calc_fitindices(mix_fit))
                    estimates <- estimates(out$model)
                    if(!is.null(estimates)){
                        estimates$Model <- this_model
                        estimates$Classes <- this_class
                    }
                    out$estimates <- estimates

                    outdat <- extract_postprob(mix_fit)
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
            dots <- list(...)
            Args_boot <- dots[which(names(dots) %in% c("replications", "previousRun", "checkHess"))]
            if(is.null(Args_boot[["replications"]])) Args_boot[["replications"]] <- 100
            Args_boot$boot <- TRUE
            for(this_mod in 1:(length(res)-1)){
                Args_boot[["base"]] <- out_list[[(this_mod+1)]][["model"]]
                Args_boot[["comparison"]] <- out_list[[this_mod]][["model"]]
                warnopt <- getOption("warn")
                options(warn = 1)
                theoutput <- capture.output({out <- tryCatch({do.call(mxCompare, Args_boot)}, error = function(e){NULL})}, type = "message")
                options(warn = warnopt)
                if(is.null(out)){
                    out_list[[(this_mod+1)]][["warns"]] <- c(out_list[[(this_mod+1)]][["warns"]], "Unable to perform BLRT.")
                } else {
                    out_list[[(this_mod+1)]]$fit["BLRT_val"] <- out$diffLL[2]
                    out_list[[(this_mod+1)]]$fit["BLRT_p"] <- out$p[2]
                }
                if(length(theoutput) > 0){
                    out_list[[(this_mod+1)]][["warnings"]] <- c(out_list[[(this_mod+1)]][["warnings"]], theoutput)
                }
            }
        }
        out_list
    }
