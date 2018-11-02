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
#' @param ... Parameters passed directly to
#' \code{\link[MplusAutomation]{mplusModeler}}. See the documentation of
#' \code{\link[MplusAutomation]{mplusModeler}}.
#' @param keepfiles Logical. Whether to retain the files created by
#' \code{mplusModeler} (e.g., for future reference, or to manually edit them).
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' results <- iris %>%
#'   select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'   estimate_profiles_mplus(n_profiles = 3, model_numbers = 1,
#'   package = "MplusAutomation")
#' }
#' @return An object of class 'tidyLPA' and 'list'
#' @importFrom methods hasArg
#' @import MplusAutomation

estimate_profiles_mplus2 <-
    function(df, n_profiles, model_numbers, ..., keepfiles = FALSE) {
        arg_list <- as.list(match.call())
        model_overall <- ifelse("model_overall" %in% names(arg_list), arg_list[["model_overall"]], "")
        filename_stem <- NULL
        if("filename_stem" %in% names(arg_list)) filename_stem <- arg_list[["filename_stem"]]
        original_names <- param_names <- names(df)

        param_names_length <- sapply(param_names, nchar)
        if (any(param_names_length > 8)) {
            warning(
                "Mplus cannot handle variable names longer than 8 characters. The following variable names were truncated:\n  ",
                paste(param_names[param_names_length > 8], collapse = "\n  ")
            )
            param_names[param_names_length > 8] <-
                substring(param_names[param_names_length > 8], 1, 8)
        }
        if (any(grepl("\\.", param_names))) {
            warning(
                "Some variable names contain periods. These were replaced with underscores for variables:\n  ",
                paste(param_names[grepl("\\.", param_names)], collapse = "\n  ")
            )
            param_names[grepl("\\.", param_names)] <-
                gsub("\\.", "_", param_names[grepl("\\.", param_names)])
        }
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
        base_object <- do.call(mplusObject, Args)

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

                out <- list(model = invisible(suppressMessages(
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

                out$fit <-
                    c(Model = this_model,
                      Classes = this_class,
                      calc_fitindices(out$model))
                estimates <- estimates(out$model)
                estimates$Model <- this_model
                estimates$Classes <- this_class
                for(which_name in 1:length(param_names)){
                    estimates$Parameter <- gsub(toupper(param_names[which_name]), original_names[which_name], estimates$Parameter)
                }
                out$estimates <- estimates

                dff <- out$model$savedata

                names(dff)[c(match(toupper(param_names), names(dff)), ncol(dff))] <-
                    c(original_names, "Class")

                dff$model_number <- this_model
                dff$classes_number <- this_class

                #dff <- reshape(dff, varying = grep("^CPROB\\d+$", names(dff), value = TRUE), timevar = "Class_prob", v.names = "Probability", direction = "long", sep = "")
                out$dff <- as.tibble(dff[, c(ncol(dff)-c(1, 0), 1:(ncol(dff)-2))])
                #out$dff <- as.tibble(dff[, match(c("model_number", "classes_number", param_names, "Class", "Class_prob", "Probability", "id"), names(dff))])

# Check for warnings ------------------------------------------------------
                warnings <- NULL
                warnings <- c(warnings, sapply(out$model$warnings, paste, collapse = " "))
                if(this_model %in% c(1, 2)){
                    warnings <- warnings[!sapply(warnings, grepl, pattern = "All variables are uncorrelated with all other variables within class.")]
                }

                if(out$fit[["prob_min"]] < .001){
                    warnings <- c(warnings, "Some classes were not assigned any cases with more than .1% probability. Consequently, these solutions are effectively identical to a solution with one class less.")
                } else {
                    if(out$fit[["n_min"]] < .01){
                        warnings <- c(warnings, "Less than 1% of cases were assigned to one of the profiles. Interpret this solution with caution and consider other models.")
                    }
                }
                # The line below needs to be fixed. CHeck what happens when there are no warnings in Mplus
                #if(out$warnings == "") out$warnings <- NULL
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
            invisible(file.remove(remove_files))
        }

        names(out_list) <-
            paste("model_", run_models$mod, "_class_", run_models$prof, sep = "")
        out_list
    }
