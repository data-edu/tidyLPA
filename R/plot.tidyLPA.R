#' @method plot tidyLPA
#' @export
plot.tidyLPA <- function(x,
                         y,
                         ...,
                         statistics = NULL) {
    if(is.null(statistics)) statistics <- "BIC"
    p <- plot_tidyLPA(data.frame(t(sapply(x, `[[`, "fit"))), statistics)
    suppressMessages(suppressWarnings(print(p)))
    invisible(p)
}


#' @method plot bestLPA
#' @export
plot.bestLPA <- function(x,
                         y,
                         ...) {
    p <- plot_tidyLPA(data.frame(x$fits), x$statistics)
    suppressMessages(suppressWarnings(print(p)))
    invisible(p)
}



plot_tidyLPA <- function(x,
                         statistics = NULL) {
    plotdat <- x
    if(is.null(statistics)) statistics <- "BIC"
    lowerbetter <- c(
        "LogLik" = " (lower is better)",
        "AIC" = " (lower is better)",
        "AWE" = " (lower is better)",
        "BIC" = " (lower is better)",
        "CAIC" = " (lower is better)",
        "CLC" = " (lower is better)",
        "KIC" = " (lower is better)",
        "SABIC" = " (lower is better"
    )
    higherbetter <- c("ICL" = " (higher is better)",
                      "Entropy" = " (higher is better")
    neutral <- c("prob_min", "prob_max", "n_min", "n_max")

    if (any(!statistics %in% c(names(lowerbetter), names(higherbetter), names(neutral)))) {
        stop("Can not plot the following statistics: ",
             paste(statistics, collapse = ", "),
             ".")
    }

    plotdat$Model <- ordered(paste("Model ", plotdat$Model))
    plotdat$Classes <- ordered(plotdat$Classes, levels = sort(unique(plotdat$Classes)))

    if (length(statistics) > 1) {
        plotdat <-
            reshape(
                plotdat[, c("Model", "Classes", statistics)],
                varying = list(Value = statistics),
                timevar = "Statistic",
                v.names = "Value",
                direction = "long"
            )
        plotdat$Statistic <-
            factor(plotdat$Statistic, labels = paste0(statistics, c(lowerbetter, higherbetter)[match(statistics, names(c(lowerbetter, higherbetter)))]))
        p <- ggplot(
            plotdat,
            aes_string(
                x = "Classes",
                y = "Value",
                color = "Model",
                group = "Model"
            )
        ) +
            geom_line(na.rm = TRUE) +
            geom_point(na.rm = TRUE) +
            theme_bw() +
            scale_color_discrete("")+
            facet_wrap(~ Statistic)

    } else {
        p <- ggplot(
            plotdat,
            aes_string(
                x = "Classes",
                y = statistics,
                color = "`Model`",
                group = "`Model`"
            )
        ) +
            geom_line(na.rm = TRUE) +
            geom_point(na.rm = TRUE) +
            ylab(paste0(statistics, c(lowerbetter, higherbetter)[match(statistics, names(c(lowerbetter, higherbetter)))])) +
            theme_bw() +
            scale_color_discrete("")
    }
    return(p)
}
