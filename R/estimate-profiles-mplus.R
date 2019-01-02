<<<<<<< HEAD
#' Estimate parameters for profiles for a specific solution (requires purchasing and installing MPlus to use)
#' @details Creates an mplus model (.inp) and associated data file (.dat)
#' @param idvar optional name of the column to be used as the ID variable (should be supplied as a string). Defaults to \code{NULL}, in which case row numbers will be used. Note the ID can be numeric or string, but must be unique.
#' @param data_filename name of data file to prepare; defaults to d.dat
#' @param script_filename name of script to prepare; defaults to i.inp
#' @param output_filename name of the output; defaults to o.out
#' @param savedata_filename name of the output for the save data (with the original data conditional probabilities); defaults to o-mod.out
#' @param starts number of initial stage starts and number of final stage optimizations; defaults to c(20, 4); can be set to be more conservative to c(500, 50)
#' @param m_iterations number of iterations for the EM algorithm; defaults to 500
#' @param st_iterations the number of initial stage iterations; defaults to 10; can be set more to be more conservative to 50
#' @param convergence_criterion convergence criterion for the Quasi-Newton algorithm for continuous outcomes; defaults to 1E-6 (.000001); can be set more conservatively to 1E-7 (.0000001)
#' @param remove_tmp_files whether to remove data, script, and output files; defaults to TRUE
#' @param print_input_file whether to print the input file to the console
#' @param return_save_data whether to return the save data (with the original data and the posterior probabilities for the classes and the class assignment) as a data.frame along with the MPlus output; defaults to TRUE
#' @param optseed random seed for analysis
#' @param cluster_ID clustering variable (i.e., if data are from students clustered into distinct classrooms) to be used as cluster variables as part of the type = complex option
#' @param include_VLMR whether to include the Vu-Lo-Mendell-Rubin likelihood-ratio test; defaults to TRUE
#' @param include_BLRT whether to include the bootstrapped LRT; defaults to FALSE because of the time this takes to run
#' @param n_processors = 1
#' @param latent_vars defaults to NULL; specification for the latent varibles as a list, i.e. list(beh = c(1, 2), cog = c(3, 4), aff = (5, 6)), where the integers represent the position of the variables passed to the function and how they correspond to the latent variables, which are named
#' @param return_all_stats defaults to FALSE; if TRUE, returns as a one-row data frame all of the statistics returned from compare_solutions_mplus()
#' @inheritParams estimate_profiles
=======
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
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc
#' @examples
#' \dontrun{
#' results <- iris %>%
#'   select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
#'   estimate_profiles_mplus(n_profiles = 3, model_numbers = 1,
#'   package = "MplusAutomation")
#' }
<<<<<<< HEAD
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

estimate_profiles_mplus <- function(df,
                                    ...,
                                    latent_vars = NULL,
                                    n_profiles,
                                    idvar = NULL,
                                    data_filename = "d.dat",
                                    script_filename = "i.inp",
                                    output_filename = "i.out",
                                    savedata_filename = "d-mod.dat",
                                    variances = "equal",
                                    covariances = "zero",
                                    model = NULL,
                                    starts = c(100, 10),
                                    m_iterations = 500,
                                    st_iterations = 20,
                                    convergence_criterion = 1E-6,
                                    remove_tmp_files = TRUE,
                                    print_input_file = FALSE,
                                    return_save_data = TRUE,
                                    optseed = NULL,
                                    n_processors = 1,
                                    cluster_ID = NULL,
                                    include_VLMR = TRUE,
                                    include_BLRT = FALSE,
                                    return_all_stats = FALSE) {

    # if (mplusAvailable() != 1) stop("It appears that MPlus is not installed; this function requires MPlus to be installed in order to work.")

    model_message <- c("The model command is deprecated in favor of the arguments for the variances and covariances. The models correspond to the following arguments for the variances and covariances:
                       Model 1: variances = 'equal'; covariances = 'zero';
                       Model 2: variances = 'equal'; covariances = 'zero';
                       Model 3: variances = 'equal'; covariances = 'equal';
                       Model 4: variances = 'varying'; covariances = 'equal' .(Cannot be estimated without MPlus);
                       Model 5: variances = 'equal'; covariances = 'varying' (Cannot be estimated without MPlus);
                       Model 6: variances = 'varying'; covariances = 'varying';
                       ")

    if (!is.null(model)) stop(model_message)

    d <- select_ancillary_functions_mplus(df, ..., cluster_ID)

    if (is.null(idvar)) {
        id <- data_frame(id = as.numeric(rownames(df)))
        idvar <- "id"
    } else {
        if (length(unique(df[[idvar]])) != length(df[[idvar]])) {
            stop("ID variable must be unique")
        }
        if (is.character(df[[idvar]])) {
            string_id <- df[[idvar]]
            num_id <- seq_along(df[[idvar]])
            id <- data_frame(id = num_id)
            names(id) <- idvar
        }
        else {
            id <- data_frame(id = df[[idvar]])
            names(id) <- idvar
        }
    }

    d <- bind_cols(id, d)

    if (!is.null(cluster_ID)) {
        d[[cluster_ID]] <- as.integer(as.factor(d[[cluster_ID]])) # MPlus requires an integer for factors
    }

    names(d) <- gsub("\\.", "_", names(d))

    x <- write_mplus(d, data_filename)

    unquoted_use_variable_names <- paste0(names(d), collapse = " ")

    if (is.null(cluster_ID)) {
        unquoted_variable_names <- paste0(names(d)[-1], collapse = " ")
    } else {
        unquoted_variable_names <- paste0(names(d)[c(-1, -ncol(d))], collapse = " ")
    }

    var_list <- vector("list", ncol(d))
    for (i in seq_along(names(d))) {
        var_list[[i]] <- names(d)[i]
    }

    # For latent variable specification

    f <- function(latent_var, for_latent_vars) {
        for_latent_vars[latent_var]
    }

    if (!is.null(latent_vars)) {
        if (!is.null(cluster_ID)) {
            for_latent_vars <- var_list[c(-1, -length(var_list))]
        } else {
            for_latent_vars <- var_list[c(-1)]
        }
        l <- list()
        for (i in 1:length(latent_vars)) {
            l[[i]] <- f(latent_vars[[i]], for_latent_vars)
        }
    }

    model <- case_when(
        variances == "equal" & covariances == "zero" ~ 1,
        variances == "varying" & covariances == "zero" ~ 2,
        variances == "equal" & covariances == "equal" ~ 3,
        variances == "varying" & covariances == "equal" ~ 4,
        variances == "equal" & covariances == "varying" ~ 5,
        variances == "varying" & covariances == "varying" ~ 6
    )

    titles <- c(
        "Equal variances and covariances fixed to 0",
        "Varying variances and covariances fixed to 0",
        "Equal variances and equal covariances",
        "Varying variances and equal covariances",
        "Equal variances and varying covariances",
        "Varying variances and varying covariances"
    )

    # consider changing to use glue()
    TITLE <- paste0("TITLE: ", titles[model])

    DATA <- paste0("DATA: File is ", data_filename, ";")

    VARIABLE_line0 <- "VARIABLE:"

    VARIABLE_line1 <- paste0("Names are ", unquoted_use_variable_names, ";")
    VARIABLE_line2 <- paste0("Classes = c(", n_profiles, ");")
    VARIABLE_line3 <- paste0("IDVARIABLE = ", idvar, ";")
    MISSING <- "Missing are all (-999);"

    if (!is.null(cluster_ID)) {
        VARIABLE_line4 <- paste0("Cluster = ", cluster_ID, ";")
    } else {
        VARIABLE_line4 <- NULL
    }

    ANALYSIS_line0 <- "ANALYSIS:"
    ANALYSIS_line1 <- "Type is mixture;"

    if (!is.null(cluster_ID)) {
        ANALYSIS_line1b <- paste0("Type is complex", ";")
    } else {
        ANALYSIS_line1b <- NULL
    }

    ANALYSIS_line2 <- paste0("starts = ", starts[1], " ", starts[2], ";")
    ANALYSIS_line3 <- paste0("miterations = ", m_iterations, ";")
    ANALYSIS_line4 <- paste0("stiterations = ", st_iterations, ";")
    ANALYSIS_line5 <- paste0("convergence = ", convergence_criterion, ";")

    if (is.null(cluster_ID)) {
        var_list <- var_list[-1]
    } else {
        var_list <- var_list[c(-1, -length(var_list))]
    }

    if (is.null(optseed)) {
        ANALYSIS_line6 <- NULL
    } else {
        ANALYSIS_line6 <- paste0("optseed = ", optseed, ";")
    }

    ANALYSIS_line7 <- paste0("processors = ", n_processors, ";")
    ANALYSIS_line8 <- paste0("stscale = 3;")

    MODEL_overall_line000 <- paste0("! model specified is: ", model)
    MODEL_overall_line00 <- paste0("MODEL:")
    MODEL_overall_line0 <- paste0("%overall%")

    # return(l)

    if (!is.null(latent_vars)) {
        MODEL_overall_line1 <- paste0("! [", unquoted_variable_names, "];")
        MODEL_overall_line2 <- paste0("! ", unquoted_variable_names, ";")

        lll <- list()
        for (i in 1:length(latent_vars)) {
            tmp <- l[[i]][1]
            tmp1 <- paste0(unlist(l[[i]][-1]), collapse = " ")
            lll[[i]] <- paste0(names(latent_vars)[i], " BY ", tmp[1], "@1 ", tmp1, ";")
        }
        MODEL_overall_line3 <- unlist(lll)

    } else {
        MODEL_overall_line1 <- paste0("[", unquoted_variable_names, "];")
        MODEL_overall_line2 <- paste0(unquoted_variable_names, ";")
        MODEL_overall_line3 <- NULL
    }

    if (include_VLMR == TRUE) {
        OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 TECH11 tech14 tech12 tech13 sampstat svalues patterns residual stdyx;"
        if (include_BLRT == TRUE) {
            OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech11 tech14 tech12 tech13 sampstat svalues patterns residual stdyx TECH14;"
        }
    } else {
        OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech14 tech12 tech13 sampstat svalues patterns residual stdyx;"
        if (include_BLRT == TRUE) {
            OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech14 tech12 tech13 sampstat svalues patterns residual stdyx TECH14;"
        }
    }

    SAVEDATA_line0 <- paste0("SAVEDATA: File is ", savedata_filename, ";")
    SAVEDATA_line1 <- "SAVE = CPROBABILITIES;"

    if (variances == "equal" & covariances == "zero") {
        model_name <- titles[1]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = FALSE, latent_vars = latent_vars)
        class_collector <- list()
        for (i in seq_len(n_profiles)) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = TRUE, latent_vars = latent_vars),
                covariances_mplus(var_list, estimate_covariance = FALSE, latent_vars = latent_vars)
            )
        }
    } else if (variances == "varying" & covariances == "zero") {
        model_name <- titles[2]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = FALSE, latent_vars = latent_vars)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = FALSE, latent_vars = latent_vars),
                covariances_mplus(var_list, estimate_covariance = FALSE, latent_vars = latent_vars)
=======
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
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc
            )
            param_names[param_names_length > 8] <-
                substring(param_names[param_names_length > 8], 1, 8)
        }
<<<<<<< HEAD
    } else if (variances == "equal" & covariances == "equal") {
        model_name <- titles[3]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = TRUE, latent_vars = latent_vars)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = TRUE, latent_vars = latent_vars),
                covariances_mplus(var_list,
                                  estimate_covariance = TRUE,
                                  param_counter = length(var_list),
                                  latent_vars = latent_vars
                )
=======
        if (any(grepl("\\.", param_names))) {
            warning(
                "Some variable names contain periods. These were replaced with underscores for variables:\n  ",
                paste(param_names[grepl("\\.", param_names)], collapse = "\n  ")
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc
            )
            param_names[grepl("\\.", param_names)] <-
                gsub("\\.", "_", param_names[grepl("\\.", param_names)])
        }
<<<<<<< HEAD
    } else if (variances == "varying" & covariances == "equal") {
        model_name <- titles[4]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = TRUE, latent_vars = latent_vars)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = FALSE, latent_vars = latent_vars),
                covariances_mplus(var_list,
                                  estimate_covariance = TRUE,
                                  param_counter = 0,
                                  latent_vars = latent_vars
                )
=======
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
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc
            )
            Args$MODEL <- NULL
        }
<<<<<<< HEAD
    } else if (variances == "equal" & covariances == "varying") {
        model_name <- titles[5]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = TRUE,latent_vars = latent_vars)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = TRUE, latent_vars = latent_vars),
                covariances_mplus(var_list, estimate_covariance = TRUE, latent_vars = latent_vars)
            )
        }
    } else if (variances == "varying" & covariances == "varying") {
        model_name <- titles[6]
        overall_collector <- covariances_mplus(var_list, estimate_covariance = TRUE, latent_vars = latent_vars)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(
                class_collector,
                make_class_mplus(var_list, class_number = i, fix_variances = FALSE, latent_vars = latent_vars),
                covariances_mplus(var_list, estimate_covariance = TRUE, latent_vars = latent_vars)
            )
        }
    }

    all_the_lines <- c(
        TITLE,
        DATA,
        VARIABLE_line0, VARIABLE_line1, VARIABLE_line2, VARIABLE_line3, VARIABLE_line4,
        MISSING,
        MODEL_overall_line00, MODEL_overall_line0, MODEL_overall_line1, MODEL_overall_line2, MODEL_overall_line3,
        overall_collector,
        class_collector,
        ANALYSIS_line0, ANALYSIS_line1, ANALYSIS_line1b, ANALYSIS_line2, ANALYSIS_line3, ANALYSIS_line4, ANALYSIS_line5, ANALYSIS_line6, ANALYSIS_line7, ANALYSIS_line8,
        OUTPUT_line0,
        SAVEDATA_line0,
        SAVEDATA_line1
    )

    # from this helpful SO answer: https://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap
    all_the_lines <- gsub("(.{1,90})(\\s|$)", "\\1\n", all_the_lines)

    cat(paste0(all_the_lines, collapse = ""),
        file = script_filename
    )

    message("Model ", paste0(model_name, " (", n_profiles, " latent profiles)"))
    x <- capture.output(MplusAutomation::runModels(target = paste0(getwd(), "/", script_filename)))
    capture <- capture.output(m <- suppressWarnings(MplusAutomation::readModels(target = paste0(getwd(), "/", output_filename))))

    if (check_warnings(m, "WARNING:  THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED") == "Warning: The best loglikelihood was not replicated") {
        warning_status <- "Warning: LL not replicated"
    } else {
        warning_status <- ""
    }

    if (check_errors(m, "THE LOGLIKELIHOOD DECREASED IN THE LAST EM ITERATION") == "Error: Convergence issue" |
        check_errors(m, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") == "Error: Convergence issue") {
        error_status <- "Error: Convergence issue"
    } else {
        error_status <- ""
    }

    if (error_status == "Error: Convergence issue" | warning_status == "Warning: LL not replicated") {
        message(str_trim(str_c(warning_status, " ", error_status)))
        return(str_trim(str_c(warning_status, " ", error_status)))
    } else if (error_status == "Warning: LL not replicated Error: Convergence issue") {
        return("Error: Convergence issue")
    } else {
        message("LogLik is ", round(abs(as.vector(get_fit_stat(m, "LL"))), 3))
        message("BIC is ", round(abs(as.vector(get_fit_stat(m, "BIC"))), 3))
        message("Entropy is ", round(abs(as.vector(get_fit_stat(m, "Entropy"))), 3))

        if (return_all_stats == TRUE) {
            n_LL_replicated <- extract_LL_mplus("i.out")
            count_LL <- count(n_LL_replicated, .data$LL)
            t <- as.character(str_c(table(m$savedata$C), collapse = ", "))
            message(paste0("Result: BIC = ", m$summaries$BIC))
=======
>>>>>>> ca12dc048a1f699748149baabb5d1be33e1365bc

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
