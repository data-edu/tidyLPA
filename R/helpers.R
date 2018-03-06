# helpers.R

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

select_create_profiles <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df <- tibble::as_tibble(df)
  df_ss <- dplyr::select(df, ..., row_number)
  cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
  d <- df_ss[cases_to_keep, ] # removes incomplete cases
  return(d)
}

select_ancillary_functions <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df <- tibble::as_tibble(df)
  df_ss <- dplyr::select(df, ...)
  cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
  d <- df_ss[cases_to_keep, ] # removes incomplete cases

  return(d)
}


select_ancillary_functions_mplus <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df <- tibble::as_tibble(df)
  df_ss <- dplyr::select(df, ...)
  cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
  d <- df_ss[cases_to_keep, ] # removes incomplete cases
  names(d) <- stringr::str_replace(names(d), "\\.", "_")
  return(d)
}

scale_vector <- function(x) {
  x / stats::sd(x, na.rm = TRUE)
}

center_vector <- function(x) {
  x - mean(x, na.rm = TRUE)
}

center_and_scale_vector <- function(x) {
  if (stats::sd(x, na.rm = TRUE) == 0) {
    x - mean(x, na.rm = TRUE)
  } else {
    (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  }
}

center_scale_function <- function(x, center_raw_data, scale_raw_data) {
  if (center_raw_data == TRUE & scale_raw_data == TRUE) {
    center_and_scale_vector(x)
  } else if (center_raw_data == TRUE) {
    center_vector(x)
  } else if (scale_raw_data == TRUE) {
    scale_vector(x)
  } else {
    x
  }
}

# addresses concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
# if (getRversion() >= "2.15.1") utils::globalVariables(c("matrix", "structure", "EEE", "EEI", "VVV", "est", "key", "model_names", "Covariance matrix structure", "n_profiles", "param_name", "posterior_prob", "profile", "val", "value", "var_name"))

#' student questionnaire data with four variables from the 2015 PISA for students in the United States
#'
#' @source http://www.oecd.org/pisa/data/
#' @format Data frame with columns
#' #' \describe{
#'   \item{broad_interest}{composite measure of students' self reported broad interest}
#'   \item{enjoyment}{composite measure of students' self reported enjoyment}
#'   \item{instrumental_mot}{composite measure of students' self reported instrumental motivation}
#'   \item{self_efficacy}{composite measure of students' self reported self efficacy}
#'   ...
#' }
#' @import tibble

"pisaUSA15"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! \nPlease report any bugs at https://github.com/jrosen48/tidyLPA or send an email to jrosen@msu.edu.")
}

extract_stats <- function(x) {
  x <- x[x != ""]
  data.frame(LL = x[1], seed = x[2], m_iterations = x[3])
}

#' Extract log-likelihoods from models fit with estimate_profiles_mplus()
#' @details Extract log-likelihoods associated with solutions from random starts from estimate_profiles_mplus(). Note that return_tmp_files = FALSE must be added to estimate_profiles_mplus() for this function to work.
#' @param output_filename name of output_filename from estimate_profiles_mplus()
#' @examples
#' \dontrun{
#' m1 <- estimate_profiles_mplus(iris,
#'                             Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'                             n_profiles = 2,
#'                             model = 1,
#'                             remove_tmp_files = FALSE)
#' extract_LL_mplus()
#' }
#' @return a tibble or a ggplot2 plot of the BIC values for the explored modelswith the log-likelihood, random start seed, and the number of the iteration
#' @export

extract_LL_mplus <- function(output_filename = "i.out") {
  raw_text <- readr::read_lines(output_filename)
  start <- which(stringr::str_detect(raw_text, "Final stage loglikelihood")) + 2
  start_vals <- raw_text[str_detect(raw_text, "start =")]
  start_vals <- stringr::str_trim(start_vals)
  start_vals <- stringr::str_sub(start_vals, end = -2L)
  start_vals <- strsplit(start_vals, "[^[:digit:]]")
  start_vals <- as.numeric(unlist(start_vals))
  start_vals <- unique(start_vals[!is.na(start_vals)])
  stop <- start + (start_vals[2] - 1)
  subset_text <- raw_text[start:stop]
  trimmed_text <- stringr::str_trim(subset_text)
  fin_text <- stringr::str_split(trimmed_text, " ")
  o <- suppressWarnings(purrr::map_df(fin_text, extract_stats))
  o$seed <- suppressWarnings(as.numeric(o$seed))
  o <- o[!is.na(o$seed), ]
  dplyr::tbl_df(o)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("Value", "se", "Class", "Variable"))

