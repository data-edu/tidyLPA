#' tidyLPA: Functionality to carry out Latent Profile Analysis in R
#'
#' Latent Profile Analysis (LPA) is a statistical modeling approach for
#' estimating distinct profiles, or groups, of variables. In the social
#' sciences and in educational research, these profiles could represent, for
#' example, how different youth experience dimensions of being engaged (i.e.,
#' cognitively, behaviorally, and affectively) at the same time.
#'
#' tidyLPA provides the functionality to carry out LPA in R. In particular,
#' tidyLPA provides functionality to specify different models that determine
#' whether and how different parameters (i.e., means, variances, and
#' covariances) are estimated and to specify (and compare solutions for) the
#' number of profiles to estimate.
#'
#' @docType package
#' @name tidyLPA
#' @import ggplot2
#' @import mclust
#' @import tibble
#' @importFrom dplyr "%>%"
#' @importFrom stats complete.cases sd reshape quantile qnorm pnorm
NULL
