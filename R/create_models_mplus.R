#' Create models for a specific mclust model
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param data_filename name of data file to prepare; defaults to d.dat
#' @param script_filename name of script to prepare; defaults to t.imp
#' @param the_title title of the model; defaults to test
#' @inheritParams create_profiles_lpa
#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble
#' @examples
#' d <- tbl_df(iris[, -5])
#' create_profiles_mplus(d,
#'                       Sepal.Length, Sepal.Width, Petal.Length,
#'                       n_profiles = 5)
#' runModels(target = paste0(getwd(), "/t.inp"))
#' m1 <- readModels((target = paste0(getwd(), "/t.out")))
#' extract_mplus_summary(m1_mplus_out)
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

create_profiles_mplus <- function(df,
                                  ...,
                                  n_profiles,
                                  the_title = "test",
                                  data_filename = "d.dat",
                                  script_filename = "t.inp",
                                  model = 1) {

    d <- select_ancillary_functions_mplus(df, ...)
    suppressWarnings(MplusAutomation::prepareMplusData(d, data_filename))

    unquoted_variable_name <- paste0(names(d), collapse = " ")

    var_list <- list()
    for (i in 1:length(names(d))) {
        var_list[[i]] <- names(d)[i]
    }

    TITLE <- paste0("TITLE: ", the_title)

    DATA <- paste0("DATA: File is ", data_filename, ";")

    VARIABLE_line0 <- "VARIABLE:"
    VARIABLE_line1 <- paste0("Names are ", unquoted_variable_name, ";")
    VARIABLE_line2 <- paste0("Classes = c(", n_profiles, ");")

    ANALYSIS_line0 <- "ANALYSIS: Type is mixture;"

    MODEL_overall_line00 <- paste0("MODEL:")
    MODEL_overall_line0 <- paste0("%overall%")
    MODEL_overall_line1 <- paste0("[", unquoted_variable_name, "];")
    MODEL_overall_line2 <- paste0(unquoted_variable_name, ";")

    OUTPUT_line0 <- "OUTPUT: TECH1 TECH11;"

    if (model == 1) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + 1]] <- paste0("%c#", i, "%")
            class_collector[[the_index + 2]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + 3]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    }

    else if (model == 2) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + i]] <- paste0("%c#", i, "%")
            class_collector[[the_index + i + 1]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 2]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    } else if (model == 3) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + i]] <- paste0("%c#", i, "%")
            class_collector[[the_index + i + 1]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 2]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    }

    readr::write_lines(c(TITLE,
                         DATA,
                         VARIABLE_line0, VARIABLE_line1, VARIABLE_line2,
                         MODEL_overall_line00, MODEL_overall_line0, MODEL_overall_line1, MODEL_overall_line2,
                         overall_collector,
                         class_collector,
                         ANALYSIS_line0,
                         OUTPUT_line0),
                       script_filename)

}

#' Extract summary statistics from an Mplus model
#' @details Extract log likelihood, BIC, and entropy statistics from an Mplus model
#' @param x an mplus model
#' @return a tibble with summary statistics
#' @export

extract_mplus_summary <- function(x) {
    x$summaries[c("LL", "BIC", "Entropy")]
}
