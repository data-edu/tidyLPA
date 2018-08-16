#' Estimate parameters for profiles for a specific solution (requires purchasing and installing MPlus to use)
#' @details Creates an mplus model (.inp) and associated data file (.dat)
#' @param idvar optional name of the column to be used as the ID variable (should be supplied as a string). Defaults to \code{NULL}, in which case row numbers will be used. Note the ID can be numeric or string, but must be unique.
#' @param data_filename name of data file to prepare; defaults to d.dat
#' @param script_filename name of script to prepare; defaults to i.inp
#' @param output_filename name of the output; defaults to o.out
#' @param savedata_filename name of the output for the save data (with the original data conditional probabilities); defaults to o-mod.out
#' @param the_title title of the model; defaults to test
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
#' @inheritParams estimate_profiles
#' @examples
#' \dontrun{
#' m <- estimate_profiles_mplus(iris,
#'                             Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'                             n_profiles = 2,
#'                             model = 1)
#' }
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

estimate_profiles_mplus <- function(df,
                                    ...,
                                    n_profiles,
                                    idvar = NULL,
                                    the_title = "test",
                                    data_filename = "d.dat",
                                    script_filename = "i.inp",
                                    output_filename = "i.out",
                                    savedata_filename = "d-mod.dat",
                                    model = 1,
                                    starts = c(100, 10),
                                    m_iterations = 500,
                                    st_iterations = 10,
                                    convergence_criterion = 1E-6,
                                    remove_tmp_files = TRUE,
                                    print_input_file = FALSE,
                                    return_save_data = TRUE,
                                    optseed = NULL,
                                    n_processors = 1,
                                    cluster_ID = NULL,
                                    include_VLMR = TRUE,
                                    include_BLRT = FALSE) {
    # message("Note that this and other functions that use MPlus are at the experimental stage! Please provide feedback at https://github.com/jrosen48/tidyLPA")

    d <- select_ancillary_functions_mplus(df, ..., cluster_ID)
    if(is.null(idvar)) {
        id <- data_frame(id = as.numeric(rownames(df)))
        idvar <- "rownum"
    } else {
        if(length(unique(df[[idvar]])) != length(df[[idvar]])) {
            stop("ID variable must be unique")
        }
        if(is.character(df[[idvar]])) {
            string_id <- df[[idvar]]
            num_id    <- seq_along(df[[idvar]])
            id <- data_frame(id = num_id)
            names(id) <- idvar
        }
        else {
            id <- data_frame(id = df[[idvar]])
            names(id) <- idvar
        }
    }
    d <- bind_cols(id, d)
    # if (!is.null(cluster_ID)) {
    #     d <- bind_cols(d, d[[cluster_ID]])
    # }
    names(d) <- gsub("\\.", "_", names(d))

    x <- write_mplus(d, data_filename)

    unquoted_variable_name <- paste0(names(d)[-1], collapse = " ")

    var_list <- vector("list", ncol(d))
    for (i in seq_along(names(d))) {
        var_list[[i]] <- names(d)[i]
    }
    titles = c("Equal variances, and covariances fixed to 0 (model 1)",
               "Equal variances, and equal covariances (model 2)",
               "Varying variances, and covariances fixed to 0 (model 3)",
               "Varying variances, and equal covariances (model 4)",
               "Equal variances, and varying covariances (model 5)",
               "Varying variances, and varying covariances (model 6)")
    TITLE <- paste0("TITLE: ", titles[model])

    DATA <- paste0("DATA: File is ", data_filename, ";")

    VARIABLE_line0 <- "VARIABLE:"
    VARIABLE_line1 <- paste0("Names are ", idvar, " ", unquoted_variable_name, ";")
    VARIABLE_line2 <- paste0("Classes = c(", n_profiles, ");")
    VARIABLE_line3 <- paste0("IDVARIABLE = ", idvar, ";")
    MISSING <- "Missing are all (-999);"

    if (!is.null(cluster_ID)) {
        VARIABLE_line4 <- paste0("Cluster = ", cluster_ID,";")
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

    var_list <- var_list[-1]

    if (is.null(optseed)) {
        ANALYSIS_line6 <- NULL
    } else {
        ANALYSIS_line6 <- paste0("optseed = ", optseed, ";")
    }

    ANALYSIS_line7 <- paste0("processors = ", n_processors, ";")

    MODEL_overall_line000 <- paste0("! model specified is: ", model)
    MODEL_overall_line00 <- paste0("MODEL:")
    MODEL_overall_line0 <- paste0("%overall%")
    MODEL_overall_line1 <- paste0("[", unquoted_variable_name, "];")
    MODEL_overall_line2 <- paste0(unquoted_variable_name, ";")

    if (include_VLMR == TRUE) {
        OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 TECH11 tech14 tech12 sampstat svalues patterns residual stdyx;"
        if (include_BLRT == TRUE) {
            OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech11 tech14 tech12 sampstat svalues patterns residual stdyx TECH14;"
        }
    } else {
        OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech14 tech12 sampstat svalues patterns residual stdyx;"
        if (include_BLRT == TRUE) {
            OUTPUT_line0 <- "OUTPUT: tech1 tech4 tech7 tech14 tech12 sampstat svalues patterns residual stdyx TECH14;"
        }
    }

    SAVEDATA_line0 <- paste0("SAVEDATA: File is ", savedata_filename, ";")
    SAVEDATA_line1 <- "SAVE = CPROBABILITIES;"

    if (model == 1) { # Varying means, equal variances, and covariances fixed to 0
        overall_collector <- covariances_mplus(var_list, estimate_covariance = F)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = T),
                                 covariances_mplus(var_list, estimate_covariance = F))
        }
    } else if (model == 3) { # Varying means, varying variances, and covariances fixed to 0
        overall_collector <- covariances_mplus(var_list, estimate_covariance = F)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = F),
                                 covariances_mplus(var_list, estimate_covariance = F))
        }
    } else if (model == 2) { # Varying means, equal variances, and equal covariances
        overall_collector <- covariances_mplus(var_list, estimate_covariance = T)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = T),
                                 covariances_mplus(var_list,
                                                 estimate_covariance = T,
                                                 param_counter = length(var_list)))
        }
    } else if (model == 4) { # Varying means, varying variances, and equal covariances
        overall_collector <- covariances_mplus(var_list, estimate_covariance = T)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = F),
                                 covariances_mplus(var_list,
                                                   estimate_covariance = T,
                                                   param_counter = 0))
        }
    } else if (model == 5) { # Varying means, equal variances, and varying covariances
        overall_collector <- covariances_mplus(var_list, estimate_covariance = T)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = T),
                                 covariances_mplus(var_list, estimate_covariance = T))
        }
    } else if (model == 6) { # Varying means, varying variances, and varying covariances
        overall_collector <- covariances_mplus(var_list, estimate_covariance = T)
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector <- c(class_collector,
                                 make_class_mplus(var_list,class_number = i,fix_variances = F),
                                 covariances_mplus(var_list, estimate_covariance = T))
        }
    }

    all_the_lines <- c(
        TITLE,
        DATA,
        VARIABLE_line0, VARIABLE_line1, VARIABLE_line2, VARIABLE_line3, VARIABLE_line4,
        MISSING,
        MODEL_overall_line00, MODEL_overall_line0, MODEL_overall_line1, MODEL_overall_line2,
        overall_collector,
        class_collector,
        ANALYSIS_line0, ANALYSIS_line1, ANALYSIS_line1b, ANALYSIS_line2, ANALYSIS_line3, ANALYSIS_line4, ANALYSIS_line5, ANALYSIS_line6, ANALYSIS_line7,
        OUTPUT_line0,
        SAVEDATA_line0,
        SAVEDATA_line1
    )

    all_the_lines <- gsub('(.{1,90})(\\s|$)', '\\1\n', all_the_lines) # from this helpful SO answer: https://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap

    cat(paste0(all_the_lines, collapse = ""),
        file = script_filename)

    x <- capture.output(MplusAutomation::runModels(target = paste0(getwd(), "/", script_filename)))
    capture <- capture.output(m <- MplusAutomation::readModels(target = paste0(getwd(), "/", output_filename)))

    if (check_warnings(m, "WARNING:  THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED.  THE") == "Warning: The best loglikelihood was not replicated") {
        warning_status <- "Warning: LL not replicated"
    } else {
        warning_status <- ""
    }

    if (check_errors(m, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY DUE TO AN INSUFFICIENT") == "Error: Convergence issue" |
        check_errors(m, "THE LOGLIKELIHOOD DECREASED IN THE LAST EM ITERATION.  CHANGE YOUR MODEL") == "Error: Convergence issue" |
        check_errors(m, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY.  ESTIMATES CANNOT") == "Error: Convergence issue" |
        check_errors(m, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY DUE TO AN ERROR IN THE") == "Error: Convergence issue") {
        error_status <- "Error: Convergence issue"
    } else {
        error_status <- ""
    }

    if (error_status == "Error: Convergence issue" | warning_status == "Warning: LL not replicated") {
        message(str_trim(str_c(warning_status, " ", error_status)))
        return(str_trim(str_c(warning_status, " ", error_status)))
    } else {
        message("LogLik is ", round(abs(as.vector(m$summaries$LL)), 3))
        message("BIC is ", round(abs(as.vector(m$summaries$BIC)), 3))
        message("Entropy is ", round(abs(as.vector(m$summaries$Entropy)), 3))
    }

    if (print_input_file == TRUE) {
        print(read_lines(script_filename))
        message("Note: This function currently prints the script output. You can also use the argument remove_tmp_files = FALSE to create the inp file, which you can then view in R Studio by clicking on the file name in the Files pane.")
    }

    if (return_save_data == TRUE) {
        x <- tbl_df(m$savedata)
        if(is.character(df[[idvar]])) {
            x[[toupper(idvar)]] <- string_id[match(x[[toupper(idvar)]], num_id)]
        }
        # x <- tbl_df(MplusAutomation::getSavedata_Data(paste0(getwd(), "/", output_filename)))

        if (remove_tmp_files == TRUE) {
            file.remove(data_filename)
            file.remove(script_filename)
            file.remove(output_filename)
            file.remove(savedata_filename)
            file.remove("Mplus Run Models.log")
        }
        fit_stats = c("LL","BIC","aBIC","AIC","Entropy")
        fs = rep(NA,length(fit_stats))
        names(fs) = fit_stats
        available_fit_stats = intersect(names(m$summaries),fit_stats)
        for (s in available_fit_stats)
            fs[s] = m$summaries[,s]
        attr(x,"fit_stats") = fs
        attr(x,"mplus_warnings") = m$warnings
        attr(x,"mplus_errors") = m$errors
        return(x)

    } else {
        if (remove_tmp_files == TRUE) {
            file.remove(data_filename)
            file.remove(script_filename)
            file.remove(output_filename)
            file.remove(savedata_filename)
            file.remove("Mplus Run Models.log")
        }

        return(m)
    }
}

check_list <- function(x, check) {
    x[1] == check
}

check_warnings <- function(x, check) {
    if (any(map_lgl(x$warnings, check_list, check = check))) {
        return(str_c("Warning: ", "The best loglikelihood was not replicated"))
    } else {
        return("No warning")
    }
}

check_errors <- function(x, check) {
    if (any(map_lgl(x$errors, check_list, check = check))) {
        return(str_c("Error: ", "Convergence issue"))
    } else {
        return("No error")
    }
}
