# helpers.R

# functions.R

select_create_profiles <- function(df, ...){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep

    # cases_to_keep <- dplyr::data_frame(row_names = 1:nrow(df_ss),
    #                                    keep = cases_to_keep)

    d <- df_ss[cases_to_keep, ] # removes incomplete cases

    return(d)
}

# quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1") utils::globalVariables(c("matrix", "structure", "EEE", "EEI", "VVV", "est", "key", "model_names"))

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
