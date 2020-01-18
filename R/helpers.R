#' @importFrom MplusAutomation mplusAvailable
.onAttach <- function(libname, pkgname) {
    has_mplus <- mplusAvailable(TRUE)
    print_message <- paste0("tidyLPA is intended for academic use. We do not make any money on this and only ask that you please cite this in publications when you use the results. You can use the function citation('tidyLPA') to create a citation. ",
                            ifelse(has_mplus == 0, "Mplus is installed; you can use package = 'MplusAutomation' when calling estimate_profiles().",
                                   "Mplus is not installed. Use only package = 'mclust' when calling estimate_profiles().")
                            )
    packageStartupMessage(print_message)
}

#' Pipe
#'
#' tidyLPA suggests using the pipe operator, \code{\%>\%}, from the magrittr
#' package (imported here from the dplyr package).
#'
#' @importFrom dplyr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs An object and a function to apply to it
#' @examples
#' # Instead of
#' subset(iris, select = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
#' # you can write
#' iris %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
NULL


quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}
