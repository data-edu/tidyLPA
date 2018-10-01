#' Plot variable means and variances by profile
#' @details Plot the variable means and variances for data frame output from estimate_profiles()
#' @param x output from estimate_profiles()
#' @details Plot the variable means and variances for data frame output from estimate_profiles().
#' When plot_what is set to 'mclust', the errorbars represent non-parametric
#' confidence intervals, obtained using bootstrapping (100 samples). Note that
#' 100 samples might be adequate for plotting, but is low for inference. If the
#' number of participants per class is highly unbalanced, then weighted likelihood
#' bootstrapping is used to ensure that each case is represented in the bootstrap
#' samples (see O'Hagan, Murphy, Scrucca, and Gormley, 2015).
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @param plot_what whether to plot tibble or mclust output from estimate_profiles(); defaults to tibble
#' @param plot_error_bars whether to plot error bars (representing the 95 percent confidence interval for the mean of each variable)
#' @param plot_rawdata whether to plot raw data; defaults to TRUE
#' @param ci confidence interval to plot (defaults to 0.95)
#' @examples
#' m3 <- estimate_profiles(iris,
#'     Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'     n_profiles = 3)
#' plot_profiles(m3)
#'
#' m3 <- estimate_profiles(iris,
#'     Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#'     n_profiles = 3, to_return = "mclust")
#' plot_profiles(m3, plot_what = "mclust")
#' @export

plot_profiles <- function(x, to_center = FALSE, to_scale = FALSE, plot_what = "tibble",
                          plot_error_bars = TRUE, plot_rawdata = TRUE,
                          ci = .95) {
  if (plot_what == "tibble") {
    n <- count(x, .data$profile)
    x <- mutate(x, profile = factor(
      .data$profile,
      labels = paste0("Profile ", n$profile, " (n = ", n$n, ")")
    ))
    if (plot_error_bars == TRUE) {
      ci <- qnorm(.5 * (1 - ci))
      x %>%
        select(-.data$posterior_prob) %>%
        mutate_at(vars(-.data$profile),
          center_scale_function,
          center_raw_data = to_center,
          scale_raw_data = to_scale
        ) %>%
        group_by(.data$profile) %>%
        summarize_all(funs(mean, sd)) %>%
        gather("key", "val", -.data$profile) %>%
        mutate(
          new_key = ifelse(str_sub(.data$key, start = -4) == "mean",
            str_sub(.data$key, start = -4),
            ifelse(str_sub(.data$key, start = -2) == "sd",
              str_sub(.data$key, start = -2),
              NA
            )
          ),
          key = ifelse(str_sub(.data$key, start = -4) == "mean",
            str_sub(.data$key, end = -6),
            ifelse(str_sub(.data$key, start = -2) == "sd",
              str_sub(.data$key, end = -4),
              NA
            )
          )
        ) %>%
        spread(.data$new_key, .data$val) %>%
        mutate(
          n_string = str_sub(as.character(.data$profile), start = 11),
          n = as.numeric(str_extract(.data$n_string, "\\-*\\d+\\.*\\d*")),
          se = ci * (.data$sd / sqrt(.data$n - 1)),
          ymin = .data$mean - .data$se,
          ymax = .data$mean + .data$se
        ) %>%
        ggplot(aes_string(
          x = "profile",
          y = "mean",
          fill = "key",
          ymin = "ymin",
          ymax = "ymax"
        )) +
        geom_col(position = "dodge") +
        geom_errorbar(position = position_dodge()) +
        scale_x_discrete("") +
        theme_bw()
    } else {
      x %>%
        select(-.data$posterior_prob) %>%
        mutate_at(vars(-.data$profile),
          scale,
          center = to_center,
          scale = to_scale
        ) %>%
        mutate(profile = as.factor(.data$profile)) %>%
        group_by(.data$profile) %>%
        summarize_all(mean) %>%
        gather("key", "val", -.data$profile) %>%
        ggplot(aes_string(
          x = "profile",
          y = "val",
          fill = "key"
        )) +
        geom_col(position = "dodge") +
        scale_x_discrete("") +
        theme_bw()
    }
  } else if (plot_what == "mclust") { # Overall this code is not *super* clear, but it's not horrible or anything either
    n_classes <- x$G
    plotdat <- x$parameters$mean
    if (to_center) {
      plotdat <- plotdat - colMeans(x$data, na.rm = TRUE)
    }
    if (to_scale) {
      plotdat <- plotdat / apply(x$data, 2, sd, na.rm = TRUE)
    }
    plotdat <- data.frame(Variable = rownames(plotdat), plotdat)
    names(plotdat)[-1] <- paste0("Value.", seq(n_classes))
    plotdat <- reshape(
      plotdat,
      direction = "long",
      varying = 2:ncol(plotdat),
      timevar = "Class"
    )[, -4]
    plotdat$Class <- ordered(plotdat$Class)
    plotdat$Variable <- ordered(plotdat$Variable, levels = colnames(x$data))

    classplot <- ggplot(NULL)
    if (plot_rawdata) {
      rawdata <- x$data
      if (to_center) {
        rawdata <- sweep(rawdata, 2, colMeans(x$data, na.rm = TRUE), "-")
      }
      if (to_scale) {
        rawdata <- sweep(rawdata, 2, apply(x$data, 2, sd, na.rm = TRUE), "/")
      }
      rawdata <- data.frame(cbind(x$z, rawdata))
      names(rawdata)[seq(n_classes)] <-
        paste0("Probability.", seq(n_classes))
      rawdata <- reshape(
        rawdata,
        direction = "long",
        varying = seq(n_classes),
        timevar = "Class"
      )[, -(length(names(rawdata)[-grep("^Probability", names(rawdata))]) + 3)]
      rawdata$Class <- ordered(rawdata$Class)
      levels(rawdata$Class) <- seq(n_classes)
      names(rawdata)[seq(ncol(x$data))] <-
        paste0("Value.", gsub("\\.", "_", colnames(x$data)))
      rawdata <-
        reshape(
          rawdata,
          direction = "long",
          varying = seq(ncol(x$data)),
          timevar = "Variable"
        )
      rawdata$Variable <- ordered(rawdata$Variable,
        levels = gsub("\\.", "_", colnames(x$data))
      )
      levels(rawdata$Variable) <- colnames(x$data)

      classplot <- classplot + geom_point(
        data = rawdata,
        position = position_jitterdodge(jitter.width = .10), # this should probably be an option
        aes_string(
          x = "Class",
          y = "Value",
          colour = "Variable",
          alpha = "Probability"
        )
      ) +
        scale_alpha_continuous(guide = FALSE, range = c(0, .1))
    }
    classplot <-
      classplot + geom_point(
        data = plotdat,
        aes(x = Class, y = Value, colour = Variable),
        position = position_dodge(width = .75),
        size = 5,
        shape = 18
      )
    # Add errorbars
    if (!is.null(ci)) {
      if (any(
        table(x$classification) / length(x$classification) <
          .5 * (1 / length(unique(x$classification)))
      )) {
        warning(
          "The number of cases per class is relatively low in some classes. Used weighted likelihood bootstrap to obtain se's."
        )
        bootstraps <-
          MclustBootstrap(x,
            nboot = 100,
            type = "wlbs",
            verbose = FALSE
          )
      } else {
        bootstraps <-
          MclustBootstrap(x,
            nboot = 100,
            type = "bs",
            verbose = FALSE
          )
      }

      ses <- data.frame(apply(bootstraps$mean, 3, function(class) {
        apply(class, 2, quantile, probs = c(
          (.5 * (1 - ci)),
          1 - (.5 * (1 - ci))
        ))
      }))
      if (to_center) {
        ses <- ses - rep(colMeans(x$data, na.rm = TRUE), each = 2)
      }
      if (to_scale) {
        ses <- ses / rep(apply(x$data, 2, sd, na.rm = TRUE), each = 2)
      }
      names(ses) <- paste0("se.", seq(n_classes))
      ses$Variable <- unlist(lapply(colnames(x$data), rep, 2))
      ses$boundary <- "lower"
      ses$boundary[seq(2, nrow(ses), by = 2)] <- "upper"
      ses <-
        reshape(
          ses,
          direction = "long",
          varying = seq(n_classes),
          timevar = "Class"
        )
      ses <-
        reshape(
          ses,
          direction = "wide",
          idvar = c("Variable", "Class"),
          timevar = "boundary"
        )
      ses$Variable <- ordered(ses$Variable, levels = colnames(x$data))
      classplot <-
        classplot + geom_errorbar(
          data = ses,
          aes_string(
            x = "Class",
            colour = "Variable",
            ymin = "se.lower",
            ymax = "se.upper"
          ),
          position = position_dodge(width = .75),
          width = .4
        )
    }
    classplot + theme_bw() +
      geom_vline(
        xintercept = seq(1.5, (n_classes - 1) + .5, 1),
        linetype = 2
      ) +
      scale_x_discrete(expand = c(0, 0))
  }
}
