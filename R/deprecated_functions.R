# bootstrap_lrt -------------------------------------------------

#' Bootstrap the likelihood-ratio test statistic for mixture components
#'
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model.
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param variances how the variable variances are estimated; defaults to "equal" (to be constant across profiles); other option is "varying" (to be varying across profiles)
#' @param covariances how the variable covariances are estimated; defaults to "zero" (to not be estimated, i.e. for the covariance matrix to be diagonal); other options are "varying" (to be varying across profiles) and "equal" (to be constant across profiles)
#' @param model which model to estimate (DEPRECATED; use variances and covariances instead)
#' @param center_raw_data logical for whether to center (M = 1) the raw data (before clustering); defaults to FALSE
#' @param scale_raw_data logical for whether to scale (SD = 1) the raw data (before clustering); defaults to FALSE
#' @param to_return character string for either "tibble" (or "data.frame") or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to TRUE
#' @param return_orig_df TRUE or FALSE (if TRUE, then the entire data.frame is returned; if FALSE, then only the variables used in the model are returned)
#' @param prior_control whether to include a regularizing prior; defaults to false
#' @param print_which_stats if set to "some", prints (as a message) the log-likelihood, BIC, and entropy; if set to "all", prints (as a message) all information criteria and other statistics about the model; if set to any other values, then nothing is printed
#' @templateVar fun bootstrap_lrt
#' @template template-depr_fun
NULL

#' @templateVar old bootstrap_lrt
#' @templateVar new estimate_profiles
#' @template template-depr_pkg
#'
#' @export
bootstrap_lrt <- function(...) {
    .Deprecated("estimate_profiles")
}

# compare_solutions_mplus -------------------------------------------------


#' Explore fit statistics various models and numbers of profiles using MPlus (requires purchasing and installing MPlus to use)
#' @details Explore the BIC values of a range of Mplus models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles_min lower bound of the number of profiles to explore; defaults to 2
#' @param n_profiles_max upper bound of the number of profiles to explore; defaults to 10
#' @param models which models to include as a list of vectors; for each vector, the first value represents how the variances are estimated and the second value represents how the covariances are estimated; defaults to list(c("equal", "zero"), c("varying", "zero"), c("equal", "equal"), c("varying", "varying"))
#' @param cluster_ID clustering variable to use as part of MPlus 'type is complex' command
#' @param save_models whether to save the models as rds files
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to TRUE
#' @param return_stats_df whether to return a list of fit statistics for the solutions explored; defaults to TRUE
#' @param dir_name character; name for directory .out files are saved to if save_models = TRUE; defaults to the present date
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
#' @param return_all_stats defaults to FALSE; if TRUE, returns as a one-row data frame all of the statistics returned from compare_solutions_mplus()
#' @return a list with a data.frame with the BIC values and a list with all of the model output; if save_models is the name of an rds file (i.e., "out.rds"), then the model output will be written with that filename and only the data.frame will be returned
#'
#' @templateVar fun compare_solutions_mplus
#' @template template-depr_fun
NULL

#' @templateVar old compare_solutions_mplus
#' @templateVar new compare_solutions
#' @template template-depr_pkg
#'
#' @export
compare_solutions_mplus <- function(...) {
    .Deprecated("compare_solutions")
}


# plot_profiles -----------------------------------------------------------

#' Plot variable means and their confidence intervals by profile for models estimated with MPlus (requires purchasing and installing MPlus to use)
#' @details Plot the variable means and variances for data frame output from estimate_profiles_mclust()
#' @param x output from estimate_profiles()
#' @param mplus_data output from estimate_profiles_mplus() with return_savedata = T specified
#' @param plot_post_probs logical; whether to plot the means and standard errors based on the posterior probabilities (if TRUE) or on the classification/profile (if FALSE); defaults to TRUE
#' @param mplus_out_name character string; name of the mplus out file that is read if the posterior probabilities are plotted; defaults to i.out (which is the default name for the file created by other tidyLPA functions)
#' @param standard_error_interval number between 0 and 1; defaults to .95
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @param plot_what whether to plot tibble or mclust output from estimate_profiles(); defaults to tibble
#' @param plot_error_bars whether to plot error bars (representing the 95 percent confidence interval for the mean of each variable)
#' @param plot_rawdata whether to plot raw data; defaults to TRUE
#' @param ci confidence interval to plot (defaults to 0.95)
#' @templateVar fun plot_profiles_mplus
#' @template template-depr_fun
NULL


#' @templateVar old plot_profiles_mplus
#' @templateVar new plot_profiles
#' @template template-depr_pkg
#'
#' @export
plot_profiles_mplus <- function(...) {
    .Deprecated("plot_profiles")
}



# estimate_profiles_mplus -------------------------------------------------
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
#' @param return_all_stats defaults to FALSE; if TRUE, returns as a one-row data frame all of the statistics returned from compare_solutions_mplus()
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param variances how the variable variances are estimated; defaults to "equal" (to be constant across profiles); other option is "varying" (to be varying across profiles)
#' @param covariances how the variable covariances are estimated; defaults to "zero" (to not be estimated, i.e. for the covariance matrix to be diagonal); other options are "varying" (to be varying across profiles) and "equal" (to be constant across profiles)
#' @param model which model to estimate (DEPRECATED; use variances and covariances instead)
#' @param center_raw_data logical for whether to center (M = 1) the raw data (before clustering); defaults to FALSE
#' @param scale_raw_data logical for whether to scale (SD = 1) the raw data (before clustering); defaults to FALSE
#' @param to_return character string for either "tibble" (or "data.frame") or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to TRUE
#' @param return_orig_df TRUE or FALSE (if TRUE, then the entire data.frame is returned; if FALSE, then only the variables used in the model are returned)
#' @param prior_control whether to include a regularizing prior; defaults to false
#' @param print_which_stats if set to "some", prints (as a message) the log-likelihood, BIC, and entropy; if set to "all", prints (as a message) all information criteria and other statistics about the model; if set to any other values, then nothing is printed
#' @param n_profiles_min lower bound of the number of profiles to explore; defaults to 2
#' @param n_profiles_max upper bound of the number of profiles to explore; defaults to 10
#' @param models which models to include as a list of vectors; for each vector, the first value represents how the variances are estimated and the second value represents how the covariances are estimated; defaults to list(c("equal", "zero"), c("varying", "zero"), c("equal", "equal"), c("varying", "varying"))
#' @param cluster_ID clustering variable to use as part of MPlus 'type is complex' command
#' @param save_models whether to save the models as rds files
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to TRUE
#' @param return_stats_df whether to return a list of fit statistics for the solutions explored; defaults to TRUE
#' @param dir_name character; name for directory .out files are saved to if save_models = TRUE; defaults to the present date
#' @examples
#' \dontrun{
#' m <- estimate_profiles_mplus(iris,
#'                             Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'                             n_profiles = 2)
#' }
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#'
#' @templateVar fun estimate_profiles_mplus
#' @template template-depr_fun
NULL

#' @templateVar old estimate_profiles_mplus
#' @templateVar new estimate_profiles
#' @template template-depr_pkg
#'
#' @export
estimate_profiles_mplus <- function(...) {
    .Deprecated("estimate_profiles")
}

