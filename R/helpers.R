# helpers.R

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

# quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1") utils::globalVariables(c("key", "val", "EEI", "EEV", "VVV", "profile"))

1:10

sd(scale(1:10, center = T, scale = T))
