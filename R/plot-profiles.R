#' @template template-plot-profiles
#' @rdname plot_profiles
#' @export
plot_profiles <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    deprecated_arguments(
        c("to_center" = "plot_profiles simply displays the data as analyzed. Center data prior to analysis.",
          "to_scale" = "plot_profiles simply displays the data as analyzed. Scale data prior to analysis.",
          "plot_what" = "tidyLPA objects now contain all information required for plotting.",
          "plot_error_bars" = "Use the 'ci' argument to specify the desired confidence intervall, or set to NULL to omit error bars.",
          "plot_rawdata" = "Renamed to rawdata."))

    UseMethod("plot_profiles", x)
}

#' @rdname plot_profiles
#' @import ggplot2
#' @export
plot_profiles.default <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    df_plot <- droplevels(x[["df_plot"]])

    if(rawdata){
        df_raw <- droplevels(x[["df_raw"]])
        # Check consistency of factor levels
        if(!all(unique(df_plot$Variable) %in% unique(df_raw$Variable))){
            stop("Could not match raw data to model estimates.")
        }
        df_raw$Variable <- as.numeric(df_raw$Variable)
    }

    level_labels <- levels(df_plot$Variable)
    df_plot$Variable <- as.numeric(df_plot$Variable)

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
            ) + scale_colour_manual(values = get_palette(max(df_plot$Classes)))
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
        scale_x_continuous(breaks = 1:length(level_labels),
                           labels = level_labels) +
        theme_bw() +
        theme(panel.grid.minor.x = element_blank())

    if(add_line) classplot <- classplot + geom_line(data = df_plot)

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

        df_plot$sd_xmin <- df_plot$Variable-.2
        df_plot$sd_xmax <- df_plot$Variable+.2
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
                    colour = "black",
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
            classplot <- classplot + facet_grid(Model ~ Classes, labeller = label_both)
        } else {
            classplot <- classplot + facet_wrap(~ Classes, labeller = label_both)
        }
    } else {
        if(length(unique(df_plot$Model)) > 1){
            classplot <- classplot + facet_wrap(~ Model, labeller = label_both)
        }
    }
    suppressWarnings(print(classplot))
    return(invisible(classplot))
}

#' @method plot_profiles tidyLPA
#' @export
plot_profiles.tidyLPA <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    Args <- as.list(match.call()[-1])
    df_plot <- get_estimates(x)

    names(df_plot)[match(c("Estimate", "Parameter"), names(df_plot))] <- c("Value", "Variable")
    df_plot$Class <- ordered(df_plot$Class)

    if(!"Classes" %in% names(df_plot)){
        df_plot$Classes <- length(unique(df_plot$Class))
    }
    # Drop useless stuff
    df_plot <- df_plot[grepl("(^Means$|^Variances$)", df_plot$Category),
                       -match(c("p"), names(df_plot))]
    df_plot$Variable <- ordered(df_plot$Variable, levels = unique(df_plot$Variable))
    # Select only requested variables, or else, all variables
    if (!is.null(variables)) {
        df_plot <- df_plot[tolower(df_plot$Variable) %in% tolower(variables), ]
    }
    df_plot$Variable <- droplevels(df_plot$Variable)
    variables <- levels(df_plot$Variable)
    df_plot$idvar <- paste0(df_plot$Model, df_plot$Classes, df_plot$Class, df_plot$Variable)
    df_plot <- reshape(data.frame(df_plot), idvar = "idvar", timevar = "Category", v.names = c("Value", "se"), direction = "wide")

    df_plot <- df_plot[, -match("idvar", names(df_plot))]
    # Get some classy names
    names(df_plot) <- gsub("\\.Means", "", names(df_plot))

    if (rawdata) {
        df_raw <- .get_long_data(x)
        df_raw <- df_raw[, c("model_number", "classes_number", variables, "Class", "Class_prob", "Probability", "id")]
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

    do.call(plot_profiles, Args)
}

#' @method plot_profiles tidyProfile
#' @export
plot_profiles.tidyProfile <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    Args <- as.list(match.call()[-1])
    df_plot <- get_estimates(x)

    df_plot$Value <- df_plot$Estimate
    df_plot$Class <- ordered(df_plot$Class)
    df_plot$Variable <- ordered(df_plot$Parameter, levels = unique(df_plot$Parameter))

    # Drop useless stuff
    df_plot <- df_plot[grepl("(^Means$|^Variances$)", df_plot$Category),
                       -match(c("p", "Parameter", "Estimate"), names(df_plot))]

    # Select only requested variables, or else, all variables
    if (!is.null(variables)) {
        df_plot <- df_plot[tolower(df_plot$Variable) %in% tolower(variables), ]
    }

    df_plot$idvar <- paste0(df_plot$Model, df_plot$Classes, df_plot$Class, df_plot$Variable)
    df_plot <- reshape(data.frame(df_plot), idvar = "idvar", timevar = "Category", v.names = c("Value", "se"), direction = "wide")

    df_plot <- df_plot[, -match("idvar", names(df_plot))]
    # Get some classy names
    names(df_plot) <- gsub("\\.Means", "", names(df_plot))

    if (rawdata) {
        df_raw <- .get_long_data(x)

        df_raw <- df_raw[, c("model_number", "classes_number", attr(x$dff, "selected"), "Class", "Class_prob", "Probability", "id")]
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

    do.call(plot_profiles, Args)
}
