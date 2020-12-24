.mplusObjectArgNames <- formalArgs(mplusObject)
.mplusModelerArgNames <- formalArgs(mplusModeler)

#' Estimate latent profiles using Mplus
#'
#' Estimates latent profiles (finite mixture models) using the commercial
#' program Mplus, through the R-interface of
#' \code{\link[MplusAutomation:mplusModeler]{MplusAutomation}}.
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
#' @param ... Parameters passed directly to
#' \code{\link[MplusAutomation]{mplusModeler}}. See the documentation of
#' \code{\link[MplusAutomation]{mplusModeler}}.
#' @param keepfiles Logical. Whether to retain the files created by
#' \code{mplusModeler} (e.g., for future reference, or to manually edit them).
#' @author Caspar J. van Lissa
#' @return An object of class 'tidyLPA' and 'list'
#' @importFrom methods hasArg
#' @import MplusAutomation
estimate_profiles_mplus2 <-
    function(df, n_profiles, model_numbers, select_vars, ..., keepfiles = FALSE) {
        arg_list <- as.list(match.call())[-1]
        df_full <- df
        df <- df[, select_vars, drop = FALSE]
        all_na_rows <- rowSums(is.na(df)) == ncol(df)
        if(any(all_na_rows)){
            warning("Data set contains cases with missing on all variables. These cases were not included in the analysis.\n")
            df <- df[!all_na_rows, , drop = FALSE]
        }
        # Always rename all variables for Mplus; simpler than handling
        # restrictions on variable name length and characters used:
        if(any(!grepl("^[a-zA-Z][a-zA-Z0-9_]+?$", names(df)))){
            stop("Mplus variable names must start with an alphabetical character, and may contain numbers and/or the underscore character (_). Some of your variables violate this rule:\n  ", paste0(names(df)[!grepl("^[a-zA-Z][a-zA-Z0-9_]+?$", names(df))], collapse = "\n  "), call. = FALSE)
        }
        if(any(duplicated(substr(names(df), 1, 8)))){
            stop("Mplus truncates variables to 8 characters. To extract the results correctly, variable names must be unique in the first 8 characters. Your variable names violating this rule are:\n  ", paste0(gsub("^(.{8})(.*)$", "\\1\\[\\2\\]", names(df))[duplicated(substr(names(df), 1, 8)) | duplicated(substr(names(df), 1, 8), fromLast = TRUE)], collapse = "\n  "), call. = FALSE)
        }
        param_names <- selected_variables <- names(df)

        # Check which arguments the user tried to pass.
        # First, check for arguments that cannot be used at all
        .estimate_profiles_mplus2ArgNames <- c("df", "n_profiles", "model_numbers", "select_vars", "keepfiles")
        if(any(!(names(arg_list) %in% c(.estimate_profiles_mplus2ArgNames, .mplusModelerArgNames, .mplusObjectArgNames)))){
            illegal_args <- names(arg_list)[which(!(names(arg_list) %in% c(.estimate_profiles_mplus2ArgNames, .mplusModelerArgNames, .mplusObjectArgNames)))]
            stop("The following illegal arguments were detected in the call to estimate_profiles:\n", paste0("  ", illegal_args, collapse = "\n"), "\nThese are not valid arguments to estimate_profiles(), mplusObject(), or mplusModeler(). Drop these arguments, and try again.", sep = "", call. = FALSE)
        }
        # Next, check for arguments that are constructed by estimate_profiles
        Args <- list(...)
        if(any(c("rdata", "usevariables", "MODEL") %in% names(Args))){
            illegal_args <- c("rdata", "usevariables", "MODEL")[which(c("rdata", "usevariables", "MODEL") %in% names(Args))]
            stop("The following illegal arguments were detected in the call to estimate_profiles:\n", paste0("  ", illegal_args, collapse = "\n"), "\nThese arguments are constructed by estimate_profiles(), and cannot be passed by users. Drop these arguments, and try again.", sep = "", call. = FALSE)
        }

        Args <- c(list(rdata = df, usevariables = param_names), Args)

        # Create necessary mplusObject arguments for mixture model, taking
        # into account any existing arguments passed by the user
        if("model_overall" %in% names(arg_list)){
            model_overall <- arg_list[["model_overall"]]
        } else {
            model_overall <- ""
        }

        filename_stem <- NULL
        if("filename_stem" %in% names(arg_list)) filename_stem <- arg_list[["filename_stem"]]
        if (hasArg("OUTPUT")) {
            Args[["OUTPUT"]] <- paste0("TECH14;\n", Args[["OUTPUT"]])
        } else {
            Args[["OUTPUT"]] <- "TECH14;\n"
        }

        if (hasArg("ANALYSIS")) {
            Args[["ANALYSIS"]] <-
                paste0("TYPE = mixture;\n", Args[["ANALYSIS"]])
        } else {
            Args[["ANALYSIS"]] <- "TYPE = mixture;\n"
        }

        char_args <- which(sapply(Args, is.character))
        Args[char_args] <-
            lapply(Args[char_args], function(x) {
                gsub("  ", "\n", x)
            })

        # Separate arguments for mplusObject and mplusModeler
        mplusObjectArgs <- Args[which(names(Args) %in% .mplusObjectArgNames)]
        mplusModelerArgs <- Args[which(names(Args) %in% .mplusModelerArgNames)]

        # Create mplusObject template for all models
        base_object <- invisible(suppressMessages(do.call(mplusObject, mplusObjectArgs)))
        # if(ncol(df) == 1){
        #     base_object$VARIABLE <- paste0("NAMES = ", names(df), ";\n")
        # }
        run_models <-
            expand.grid(prof = n_profiles, mod = model_numbers)

        out_list <- mapply(
            FUN = function(this_class, this_model) {
                # Generate specific Mplus object for each individual model
                base_object$VARIABLE <-
                    paste0(base_object$VARIABLE, paste(c(
                        "CLASSES = ",
                        paste(
                            "c1(",
                            this_class,
                            ")",
                            sep = "",
                            collapse = " "
                        ),
                        ";\n"
                    ), collapse = ""))
                model_class_specific <-
                    gsub("  ",
                         "\n",
                         syntax_class_specific(this_model, param_names))
                expand_class_specific <- ""

                for (this_class in 1:this_class) {
                    expand_class_specific <-
                        paste0(expand_class_specific,
                               gsub("\\{C\\}", this_class, paste(
                                   c(
                                       "%c1#",
                                       this_class,
                                       "%\n",
                                       model_class_specific,
                                       "\n\n"
                                   ),
                                   collapse = ""
                               )))
                }
                base_object$MODEL <-
                    paste0(base_object$MODEL,
                           expand_class_specific)

                base_object$SAVEDATA <-
                    paste0(
                        "FILE IS ",
                        paste0(
                            ifelse(
                                !is.null(filename_stem),
                                paste0(filename_stem, "_"),
                                ""
                            ),
                            "model_",
                            this_model,
                            "_class_",
                            this_class
                        ),
                        ".dat;\nSAVE = cprobabilities;"
                    )

                base_object$TITLE <-
                    trimws(paste(
                        ifelse(!is.null(filename_stem), filename_stem, ""),
                        "model",
                        this_model,
                        "with",
                        this_class,
                        "classes"
                    ))


                # Run analysis ------------------------------------------------------------


                filename = c(inp = ifelse(
                    !is.null(filename_stem),
                    paste0(
                        paste(
                            filename_stem,
                            "model",
                            this_model,
                            "class",
                            this_class,
                            sep = "_"
                        ),
                        ".inp"
                    ),
                    paste0(
                        paste("model", this_model, "class", this_class, sep = "_"),
                        ".inp"
                    )
                ))

                out <- list(model = quiet(suppressMessages(
                    do.call(what = mplusModeler,
                            args = c(
                                list(
                                    object = base_object,
                                    dataout = ifelse(
                                        !is.null(filename_stem),
                                        paste0("data_", filename_stem, ".dat"),
                                        "data.dat"
                                    ),
                                    modelout = filename["inp"],
                                    run = 1L,
                                    check = FALSE,
                                    varwarnings = TRUE,
                                    writeData = "ifmissing",
                                    hashfilename = TRUE
                                ),
                                mplusModelerArgs
                            ))
                ))$results)

                warnings <- NULL
                if(!is.null(out$model$summaries$LL)){
                    out$fit <-
                        c(Model = this_model,
                          Classes = this_class,
                          calc_fitindices(out$model))
                    estimates <- estimates(out$model)
                    if(!is.null(estimates)){
                        estimates$Model <- this_model
                        estimates$Classes <- this_class
                    }
                    out$estimates <- estimates

                    if(!is.null(out$model[["savedata"]])){
                        #dff <- out$model$savedata
                        outdat <- as.matrix(out$model$savedata[, grep("CPROB1", names(out$model$savedata)):ncol(out$model$savedata)])
                        dff <- matrix(NA_real_, dim(df_full)[1], dim(outdat)[2])
                        dff[!all_na_rows, ] <- outdat
                        colnames(dff) <- c(paste0("CPROB", 1:(ncol(dff)-1)), "Class")

                        out$dff <- as_tibble(cbind(df_full, dff))
                        out$dff$model_number <- this_model
                        out$dff$classes_number <- this_class
                        out$dff <- out$dff[, c((ncol(out$dff)-1), ncol(out$dff), 1:(ncol(out$dff)-2))]
                        attr(out$dff, "selected") <- selected_variables
                    }

                    # Check for warnings ------------------------------------------------------

                    if(!is.na(out$fit[["prob_min"]])){
                        if(out$fit[["prob_min"]]< .001) warnings <- c(warnings, "Some classes were not assigned any cases with more than .1% probability. Consequently, these solutions are effectively identical to a solution with one class less.")
                    }
                    if(!is.na(out$fit[["n_min"]])){
                        if(out$fit[["n_min"]] < .01) warnings <- c(warnings, "Less than 1% of cases were assigned to one of the profiles. Interpret this solution with caution and consider other models.")
                    }
                } else {
                    out$fit <-
                        c(Model = this_model,
                          Classes = this_class,
                          "LogLik" = NA, "AIC" = NA, "AWE" = NA, "BIC" = NA, "CAIC" = NA, "CLC" = NA, "KIC" = NA, "SABIC" = NA, "ICL" = NA, "Entropy" = NA, "prob_min" = NA, "prob_max" = NA, "n_min" = NA, "n_max" = NA, "BLRT_val" = NA, "BLRT_p" = NA)
                    out$estimates <- NULL
                }

                warnings <- unlist(c(warnings, sapply(out$model$warnings, paste, collapse = " "), sapply(out$model$errors, paste, collapse = " ")))
                warnings <- warnings[!grepl("first 8 characters", warnings)]
                if(this_class == 1){
                    warnings <- warnings[!grepl("TECH14 option is not available for TYPE=MIXTURE with only one class.", warnings)]

                }
                if(this_model %in% c(1, 2, 6)){
                    warnings <- warnings[!grepl("All variables are uncorrelated with all other variables within class.", warnings)]
                }
                if(length(warnings)) out$warnings <- warnings
                class(out) <- c("tidyProfile.mplus", "tidyProfile", "list")
                out
            },
            this_class = run_models$prof,
            this_model = run_models$mod,
            SIMPLIFY = FALSE
        )


        all_files <-
            paste0(
                ifelse(!is.null(filename_stem), paste0(filename_stem, "_"), ""),
                paste("model_", run_models$mod, "_class_", run_models$prof, sep = "")
            )
        if (!keepfiles) {
            remove_files <-
                c(
                    out_list[[1]]$model$input$data$file,
                    paste0(all_files, ".inp"),
                    paste0(all_files, ".out"),
                    paste0(all_files, ".dat")
                )
            remove_files <- remove_files[which(remove_files %in% list.files())]
            if(length(remove_files) > 0){
                invisible(file.remove(remove_files))
            }
        }

        names(out_list) <-
            paste("model_", run_models$mod, "_class_", run_models$prof, sep = "")
        out_list
    }
