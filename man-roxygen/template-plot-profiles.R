#' Create latent profile plots
#'
#' Creates a profile plot according to best practices, focusing on the
#' visualization of classification uncertainty by showing:
#' \enumerate{
#' \item Bars reflecting a confidence interval for the class centroids
#' \item Boxes reflecting the standard deviations within each class; a box
#' encompasses +/- 64\% of the observations in a normal distribution
#' \item Raw data, whose transparancy is weighted by the posterior class
#' probability, such that each datapoint is most clearly visible for the class
#' it is most likely to be a member of.
#' }
#' @param x An object containing the results of a mixture model analysis.
#' @param variables A character vectors with the names of the variables to be
#' plotted (optional).
#' @param ci Numeric. What confidence interval should the errorbars span?
#' Defaults to a 95\% confidence interval. Set to NULL to remove errorbars.
#' @param sd Logica. Whether to display a box encompassing +/- 1SD Defaults to
#' TRUE.
#' @param rawdata Should raw data be plotted in the background? Setting this to
#' TRUE might result in long plotting times.
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param alpha_range The minimum and maximum values of alpha (transparancy) for
#' the raw data. Minimum should be 0; lower maximum values of alpha can help
#' reduce overplotting.
#' @param ... Arguments passed to and from other functions.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @keywords plot mixture
#' @examples
#' \dontrun{
#' mtcars %>%
#'   select(wt, qsec, drat) %>%
#'   poms %>%
#'   estimate_profiles(1:4) %>%
#'   plotMixtures
#' }
