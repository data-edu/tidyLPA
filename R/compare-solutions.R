#' Explore BIC for various models and numbers of profiles
#' @details Explore the BIC values of a range of models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 1 through 9 (1:9)
#' @param statistic what statistic to plot; BIC or ICL are presently available as options
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output instead of a plot; defaults to FALSE
#' @param models which models to include as a list of vectors; for each vector, the first value represents how the variances are estimated and the second value represents how the covariances are estimated; defaults to list(c("equal", "zero"), c("varying", "zero"), c("equal", "equal"), c("varying", "varying"))
#' @inheritParams estimate_profiles
#' @return a ggplot2 plot of the BIC values for the explored models
#' @examples
#' compare_solutions(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' @export

compare_solutions <- function(df, ...,
                              n_profiles_range = 1:9,
                              models = list(
                                c("equal", "zero"),
                                c("varying", "zero"),
                                c("equal", "equal"),
                                c("varying", "varying")
                              ),
                              center_raw_data = FALSE,
                              scale_raw_data = FALSE,
                              statistic = "BIC",
                              return_table = FALSE,
                              prior_control = F) {
  d <- select_ancillary_functions(df, ...)

  if (center_raw_data == TRUE | scale_raw_data == TRUE) {
    d <- mutate_all(d,
      center_scale_function,
      center_raw_data = center_raw_data,
      scale_raw_data = scale_raw_data
    )
  }

  variances <- map_chr(models, ~.[1])
  covariances <- map_chr(models, ~.[2])

  model <- case_when(
    variances == "equal" & covariances == "zero" ~ "EEI",
    variances == "varying" & covariances == "zero" ~ "VVI",
    variances == "equal" & covariances == "equal" ~ "EEE",
    # variances == "varying" & covariances == "equal" ~ 4, # I'd remove
    # variances == "equal" & covariances == "varying" ~ 5,
    variances == "varying" & covariances == "varying" ~ "VVV"
  )

  titles <- c(
    "Equal variances and covariances fixed to zero (model 1)",
    "Varying variances and covariances fixed to zero (model 2)",
    "Equal variances and equal covariances (model 3)",
    # "Varying variances and equal covariances (model 4)",
    # "Equal variances and varying covariances (model 5)",
    "Varying variances and varying covariances (model 6)"
  )

  title <- titles[model]

  if (prior_control == FALSE) {
    if (statistic == "BIC") {
      x <- suppressWarnings(mclustBIC(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE
      ))
    } else if (statistic == "ICL") {
      x <- suppressWarnings(mclustICL(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE
      ))
    } else {
      stop("This statistic cannot presently be computed")
    }
  } else {
    if (statistic == "BIC") {
      x <- suppressWarnings(mclustBIC(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE,
        prior = priorControl()
      ))
    } else if (statistic == "ICL") {
      x <- suppressWarnings(mclustICL(d,
        G = n_profiles_range,
        modelNames = model,
        warn = TRUE,
        verbose = FALSE,
        prior = priorControl()
      ))
    } else {
      stop("This statistic cannot presently be computed")
    }
  }

  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles")

  to_plot <- y %>%
    gather("Model", "val", -.data$n_profiles) %>%
    mutate(
      "Model" = as.factor(.data$`Model`),
      val = abs(.data$val)
    )

  if (return_table == TRUE) {
    return(to_plot)
  }

  to_plot$n_profiles <- as.factor(to_plot$n_profiles)

  p <- ggplot(to_plot, aes_string(
    x = "n_profiles",
    y = "val",
    color = "`Model`",
    group = "`Model`"
  )) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ylab(paste0(statistic, " (smaller value is better)")) +
    theme_bw() +
    ggplot2::scale_color_discrete("", labels = titles)
    xlab("Profiles")

  p
}
