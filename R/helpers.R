# helpers.R

select_create_profiles <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df <- as_tibble(df)
  df_ss <- select(df, ..., row_number)
  cases_to_keep <- complete.cases(df_ss) # to use later for comparing function to index which cases to keep
  d <- df_ss[cases_to_keep, ] # removes incomplete cases
  return(d)
}

select_ancillary_functions <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df <- as_tibble(df)
  df_ss <- select(df, ...)
  cases_to_keep <- complete.cases(df_ss) # to use later for comparing function to index which cases to keep
  d <- df_ss[cases_to_keep, ] # removes incomplete cases

  return(d)
}

select_ancillary_functions_mplus <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
  df %>%
    as_tibble() %>%
    select(...)
}

scale_vector <- function(x) {
  x / sd(x, na.rm = TRUE)
}

center_vector <- function(x) {
  x - mean(x, na.rm = TRUE)
}

center_and_scale_vector <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    x - mean(x, na.rm = TRUE)
  } else {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
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

# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("\nPlease report any issues or feature requests at https://github.com/jrosen48/tidyLPA or via email to tidylpa@googlegroups.com.")
# }

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
#'                             remove_tmp_files = FALSE)
#' extract_LL_mplus()
#' }
#' @return a tibble or a ggplot2 plot of the BIC values for the explored modelswith the log-likelihood, random start seed, and the number of the iteration
#' @export

extract_LL_mplus <- function(output_filename = "i.out") {
  raw_text <- readr::read_lines(output_filename)
  start <- which(stringr::str_detect(raw_text, "Final stage loglikelihood")) + 2
  start_vals <- raw_text[stringr::str_detect(raw_text, "starts =")]
  start_vals <- stringr::str_trim(start_vals)
  start_vals <- stringr::str_sub(start_vals, end = -2L)
  start_vals <- stringr::str_split(start_vals, "[^[:digit:]]")
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

if (getRversion() >= "2.15.1") globalVariables(c("Value", "se", "Class", "Variable", "."))

write_mplus <- function(d, file_name, na_string = "-999", ...) {
  write.table(d,
    file = file_name,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    na = as.character(na_string),
    ...
  )
}

make_class_mplus <- function(var_list, class_number, fix_variances = F) {
  class_init <- vector(length = 3, mode = "list")
  class_init[[1]] <- paste0("%c#", class_number, "%")
  class_init[[2]] <- paste0("[", paste(var_list, collapse = " "), "];")
  class_init[[3]] <- paste0(
    paste(var_list, collapse = " "),
    ifelse(fix_variances,
      paste0("(1-", length(var_list), ")"),
      ""
    ),
    ";"
  )
  return(class_init)
}

covariances_mplus <- function(var_list, estimate_covariance = F, param_counter = NULL) {
  combine2 <- utils::combn(length(var_list), 2)
  variances <- vector(length = ncol(combine2), mode = "list")

  for (k in 1:ncol(combine2)) {
    variances[[k]] <- paste0(
      var_list[[combine2[1, k]]],
      " WITH ",
      var_list[[combine2[2, k]]],
      ifelse(estimate_covariance, "", "@0"),
      ifelse(is.null(param_counter),
        "",
        paste0(" (", param_counter + k, ")")
      ),
      ";"
    )
  }
  return(variances)
}


get_fit_stat <- function(m, stat) {
  return(ifelse(stat %in% names(m$summaries),
    m$summaries[[stat]],
    NA
  ))
}
