#' Create latent profile plots
#'
#' Creates a profile plot according to best practices, focusing on the
#' visualization of classification uncertainty by showing:
#' \enumerate{
#' \item Bars reflecting a confidence interval for the class centroids
#' \item Boxes reflecting the standard deviations within each class; a box
#' encompasses +/- 64% of the observations in a normal distribution
#' \item Raw data, whose transparancy is weighted by the posterior class
#' probability, such that each datapoint is most clearly visible for the class
#' it is most likely to be a member of.
#' }
#' @param x An object containing the results of a mixture model analysis.
#' @param variables A character vectors with the names of the variables to be
#' plotted (optional).
#' @param ci Numeric. What confidence interval should the errorbars span? Defaults to
#' a 95\% confidence interval. Set to NULL to remove errorbars.
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
#' @export
#' @import ggplot2
#' @keywords plot mixture
#' @examples
#' \dontrun{
#' createMixtures(classes = 1:4, filename_stem = "cars",
#'                model_overall = "wt ON drat;",
#'                model_class_specific = "wt;  qsec;",
#'                rdata = mtcars,
#'                usevariables = c("wt", "qsec", "drat"),
#'                OUTPUT = "standardized")
#' runModels(replaceOutfile = "modifiedDate")
#' cars_results <- readModels(filefilter = "cars")
#' plotMixtures(cars_results, rawdata = TRUE)
#' }
#' \dontrun{
#' plotMixtures(cars_results, variables = "wt")
#' }
#' \dontrun{
#' plotMixtures(cars_results, coefficients = "stdyx.standardized")
#' }
plotMixtures <- function(x, variables = NULL, ci = .95, sd = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    deprecated_arguments(
        c("to_center" = "plotMixtures simply displays the data as analyzed. Center data prior to analysis.",
        "to_scale" = "plotMixtures simply displays the data as analyzed. Scale data prior to analysis.",
        "plot_what" = "tidyLPA objects now contain all information required for plotting.",
        "plot_error_bars" = "Use the 'ci' argument to specify the desired confidence intervall, or set to NULL to omit error bars.",
        "plot_rawdata" = "Renamed to rawdata."))

    UseMethod("plotMixtures", x)
}

#' @rdname plotMixtures
#' @export
plot_profiles <- plotMixtures

#' @rdname plotMixtures
#' @export
plotMixtures.default <- function(x, variables = NULL, ci = .95, sd = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    df_plot <- x[["df_plot"]]
    df_raw <- x[["df_raw"]]

    # Basic plot
    if (bw) {
        classplot <-
            ggplot(NULL,
                   aes_string(
                       x = "Variable",
                       y = "Value",
                       group = "Class",
                       linetype = "Class",
                       shape = "Class"
                   ))
    } else {
        classplot <-
            ggplot(
                NULL,
                aes_string(
                    x = "Variable",
                    y = "Value",
                    group = "Class",
                    linetype = "Class",
                    shape = "Class",
                    colour = "Class"
                )
            )
    }

    if (rawdata) {
        classplot <- classplot +
            geom_jitter(
                data = df_raw,
                width = .2,
                aes_string(
                    x = "Variable",
                    y = "Value",
                    shape = "Class",
                    alpha = "Probability"
                )
            ) +
            scale_alpha_continuous(range = alpha_range, guide = FALSE)
    }
    classplot <- classplot + geom_point(data = df_plot) +
        geom_line(data = df_plot) +
        theme_bw()
    # Add errorbars
    if (!is.null(ci)) {
        ci <- qnorm(.5 * (1 - ci))
        df_plot$error_min <- df_plot$Value + ci*df_plot$se
        df_plot$error_max <- df_plot$Value - ci*df_plot$se

        classplot <-
            classplot + geom_errorbar(data = df_plot,
                                      aes_string(ymin = "error_min",
                                                 ymax = "error_max"),
                                      width = .4)
    }
    if(sd){

        df_plot$sd_xmin <- as.numeric(df_plot$Variable)-.2
        df_plot$sd_xmax <- as.numeric(df_plot$Variable)+.2
        df_plot$sd_ymin <- df_plot$Value - sqrt(df_plot$Value.Variances)
        df_plot$sd_ymax <- df_plot$Value + sqrt(df_plot$Value.Variances)

        if(bw){
            classplot <-
                classplot + geom_rect(
                    data = df_plot,
                    aes_string(
                        xmin = "sd_xmin",
                        xmax = "sd_xmax",
                        ymin = "sd_ymin",
                        ymax = "sd_ymax",
                        linetype = "Class"
                    ),
                    fill=ggplot2::alpha("grey", 0),
                    inherit.aes=FALSE
                )
        } else {
            classplot <-
                classplot + geom_rect(
                    data = df_plot,
                    aes_string(
                        xmin = "sd_xmin",
                        xmax = "sd_xmax",
                        ymin = "sd_ymin",
                        ymax = "sd_ymax",
                        colour = "Class"
                    ),
                    fill=ggplot2::alpha("grey", 0),
                    inherit.aes=FALSE
                )
        }

    }
    if (length(unique(df_plot$Classes)) > 1) {
        if(length(unique(df_plot$Model)) > 1){
            classplot <- classplot + facet_wrap(Model ~ Classes)
        } else {
            classplot <- classplot + facet_wrap(~ Classes)
        }
    } else {
        if(length(unique(df_plot$Model)) > 1){
            classplot <- classplot + facet_wrap(~ Model)
        }
    }
    suppressWarnings(print(classplot))
    return(invisible(classplot))
}

#' @rdname plotMixtures
#' @export
plotMixtures.tidyLPA <- function(x, variables = NULL, ci = .95, sd = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    Args <- as.list(match.call()[-1])

    df_plot <- get_estimates(x)

    df_plot$Value <- df_plot$Estimate
    df_plot$Class <- ordered(df_plot$Class)
    df_plot$Variable <- ordered(df_plot$Parameter, levels = unique(df_plot$Parameter))


    # Drop useless stuff
    df_plot <-
        subset(
            df_plot,
            subset = grepl("(^Means$|^Variances$)", df_plot$Category),
            select = -match(c("p", "Parameter", "Estimate"), names(df_plot))
        )

    # Select only requested variables, or else, all variables
    if (!is.null(variables)) {
        df_plot <- df_plot[tolower(df_plot$Variable) %in% tolower(variables), ]
    }

    df_plot$idvar <- paste0(df_plot$Model, df_plot$Classes, df_plot$Class, df_plot$Variable)
    df_plot <- reshape(data.frame(df_plot), idvar = "idvar", timevar = "Category", v.names = c("Value", "se"), direction = "wide")

    df_plot <- df_plot[, -5]
    # Get some classy names
    names(df_plot)[5:6] <- c("Value", "se")

    if (rawdata) {
        df_raw <- data.frame(get_data(x))
        df_raw$Class <- ordered(df_raw$Class_prob, levels = levels(df_plot$Class))
        variable_names <- paste("Value", names(df_raw)[-c(1,2, ncol(df_raw)-c(0:3))], sep = "...")
        names(df_raw)[-c(1,2, ncol(df_raw)-c(0:3))] <- variable_names
        df_raw <- reshape(
            df_raw,
            varying = c(Variable = variable_names),
            idvar = "new_id",
            direction = "long",
            timevar = "Variable",
            sep = "..."
        )
        if(any(c("Class_prob", "id", "new_id") %in% names(df_raw))){
            df_raw <- df_raw[, -which(names(df_raw) %in% c("Class_prob", "id", "new_id"))]
        }

        df_raw$Variable <- ordered(df_raw$Variable,
                                    levels = levels(df_plot$Variable))
        names(df_raw)[c(1,2)] <- c("Model", "Classes")
    } else {
        df_raw <- NULL
    }

    Args[["x"]] <- list(df_plot = df_plot, df_raw = df_raw)
    #return(Args$x)
    do.call(plotMixtures, Args)
}
