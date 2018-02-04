# helpers.R

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
#'   \item{CNTSTUID}{international student ID}
#'   \item{SCHID}{international school ID}
#'   ...
#' }
#' @import tibble

"pisaUSA15"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! \nPlease report any bugs at https://github.com/jrosen48/tidyLPA or send an email to jrosen@msu.edu.")
}
