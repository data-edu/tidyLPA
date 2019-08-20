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
        arg_list <- as.list(match.call())
        df_full <- df
        df <- df[, select_vars, drop = FALSE]
        all_na_rows <- rowSums(is.na(df)) == ncol(df)
        if(any(all_na_rows)){
            warning("Data set contains cases with missing on all variables. These cases were not included in the analysis.\n")
        }
        df <- df[!all_na_rows, , drop = FALSE]
        model_overall <- ifelse("model_overall" %in% names(arg_list), arg_list[["model_overall"]], "")
        filename_stem <- NULL
        if("filename_stem" %in% names(arg_list)) filename_stem <- arg_list[["filename_stem"]]
        original_names <- param_names <- names(df)
        param_warning <- ""
        param_names_length <- sapply(param_names, nchar)
        if (any(param_names_length > 8)) {
            param_warning <- "\nMplus cannot handle variable names longer than 8 characters. Some variable names were truncated.\n"
            param_names[param_names_length > 8] <-
                substring(param_names[param_names_length > 8], 1, 8)
        }
        if (any(grepl("\\.", param_names))) {
            param_warning <- paste0(param_warning, "Some variable names contained periods. These were replaced with underscores.\n")
            param_names[grepl("\\.", param_names)] <-
                gsub("\\.", "_", param_names[grepl("\\.", param_names)])
        }
        if(!length(unique(param_names)) == length(param_names)){
            dups <- duplicated(param_names) | duplicated(param_names, fromLast = TRUE)
            stop(param_warning, "\nThe resulting variable names were not unique. Please rename your variables prior to running the analysis, to prevent duplicates. The duplicated variable names are:\n\n",
                 gsub(", ", "", toString(t(cbind(paste0(c("Original:", names(df)[dups]), "\t"), paste0(c("Renamed:", param_names[dups]), "\n"))))))
        }
        selected_variables <- names(df)
        names(df) <- param_names

        Args <- c(list(
            rdata = df,
            usevariables = param_names,
            OUTPUT = "TECH14;"
        ),
        list(...))
        if (hasArg("MODEL")) {
            warning(
                "MODEL argument was dropped: createMixtures constructs its own MODEL argument from model_overall and model_class_specific."
            )
            Args$MODEL <- NULL
        }

        if (hasArg("model_overall")) {
            Args[["MODEL"]] <-
                paste(c("%OVERALL%\n", model_overall, "\n\n"), collapse = "")
        } else {
            Args[["MODEL"]] <- ""
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



        # Create mplusObject template
        base_object <- invisible(suppressMessages(do.call(mplusObject, Args)))
        if(ncol(df) == 1){
            base_object$VARIABLE <- paste0("NAMES = ", names(df), ";\n")
        }
        run_models <-
            expand.grid(prof = n_profiles, mod = model_numbers)

        out_list <- mapply(
            FUN = function(this_class, this_model) {
                # Generate specific Mplus object ------------------------------------------

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
                    mplusModeler(
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
                        Mplus_command = "Mplus",
                        writeData = "ifmissing",
                        hashfilename = TRUE
                    )
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
                        for(which_name in 1:length(param_names)){
                            estimates$Parameter <- gsub(toupper(param_names[which_name]), original_names[which_name], estimates$Parameter)
                        }
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
                if(this_class == 1){
                    warnings <- warnings[!sapply(warnings, grepl, pattern = "TECH14 option is not available for TYPE=MIXTURE with only one class.")]

                }
                if(this_model %in% c(1, 2)){
                    warnings <- warnings[!sapply(warnings, grepl, pattern = "All variables are uncorrelated with all other variables within class.")]
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
