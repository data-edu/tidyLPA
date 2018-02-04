#' Bootstrap the likelihood-ratio test statistic for mixture components
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model.
#' @inheritParams estimate_profiles
#' @importFrom rlang .data
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
                          model = 1) {
  message("Note. This function is still in-development and may cause unexpected errors.")

  d <- select_ancillary_functions(df, ...)

  if (model == 1) {
    model <- "EEI"
  } else if (model == 2) {
    model <- "EEE"
  } else if (model == 3) {
    model <- "VVV"
  } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
    model <- model
  } else {
    stop("Model name is not correctly specified: use 1, 2, or 3 (see ?estimate_profiles for descriptions) or one of the model names specified from mclustModelNames() from mclust")
  }

  mclust::mclustBootstrapLRT(data = d, modelName = model)

  # if (length(model_names) == 1) {
  #     mclust::mclustBootstrapLRT(data = df, modelName = model_names, ...)
  # } else if (length(model_names) > 1) {
  #     purrr::map(model_names, ~ mclust::mclustBootstrapLRT(data = df, modelName = .))
  # }
}
