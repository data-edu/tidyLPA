#' Bootstrap the likelihood-ratio test statistic for mixture components
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model.
#' @inheritParams estimate_profiles
#' @examples
#' \dontrun{
#' d <- pisaUSA15
#' d <- dplyr::sample_n(d, 200)
#' bootstrap_lrt(d,
#'               broad_interest, enjoyment, self_efficacy)
#' }
#' @export

bootstrap_lrt <- function(df,
                          ...,
                          n_profiles,
                          variances = "fixed",
                          covariances = "zero"
                          ) {
  message("Note. This function is still in-development and may cause unexpected errors.")

  d <- select_ancillary_functions(df, ...)

  # When you have a bunch of if else if statements like this you could
  # consider using `switch` instead, which I think tends to be a little
  # more readable (does nothing for the functionality that I know of though)
  if (variances == "fixed" & covariances == "zero") {
      model <- "EEI"
  } else if (variances == "fixed" & covariances == "fixed") {
      model <- "EEE"
  } else if (variances == "freely-estimated" & covariances == "zero") {
      model <- "VVI"
  } else if (variances == "freely-estimated" & covariances == "freely-estimated") {
      model <- "VVV"
  } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
      model <- model
  } else {

      # looks like this needs to be updated, correct? Not using model names
      # anymore I don't think...
      stop("Model name is not correctly specified: use 1, 2, 3, or 6 (see  ?estimate_profiles for descriptions) or one of the model names specified from mclustModelNames() from mclust")
  }

  mclustBootstrapLRT(data = d, modelName = model)

  # Assuming the below is archived code. I sometimes do this as well but it
  # might be better to move it into a separate file before sending this out
  # for review

  # if (length(model_names) == 1) {
  #     mclustBootstrapLRT(data = df, modelName = model_names, ...)
  # } else if (length(model_names) > 1) {
  #     map(model_names, ~ mclustBootstrapLRT(data = df, modelName = .))
  # }
}
