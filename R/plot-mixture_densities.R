#' Create density plots for mixture models
#'
#' Creates a faceted plot of density plots for an object of class 'tidyLPA'. For
#' each variable, a Total density plot will be shown, along with separate
#' density plots for each latent class, where cases are weighted by the
#' posterior probability of being assigned to that class.
#' @param x Object to plot.
#' @param variables Which variables to plot. If NULL, plots all variables that
#' are present in all Mplus models.
#' @param bw Logical. Whether to make a black and white plot (for print) or a
#' color plot. Defaults to FALSE, because these density plots are hard to read
#' in black and white.
#' @param conditional Logical. Whether to show a conditional density plot
#' (surface area is divided amongst the latent classes), or a classic density
#' plot (surface area of the total density plot is equal to one, and is
#' subdivided amongst the classes).
#' @param alpha Numeric (0-1). Only used when bw and conditional are FALSE. Sets
#' the transparency of geom_density, so that classes with a small number of
#' cases remain visible.
#' @param facet_labels Named character vector, the names of which should
#' correspond to the facet labels one wishes to rename, and the values of which
#' provide new names for these facets. For example, to rename variables, in the
#' example with the 'iris' data below, one could specify:
#' \code{facet_labels = c("Pet_leng" = "Petal length")}.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @import ggplot2
#' @keywords mixture density plot
#' @examples
#' \dontrun{
#' results <- iris %>%
#'   subset(select = c("Sepal.Length", "Sepal.Width",
#'     "Petal.Length", "Petal.Width")) %>%
#'   estimate_profiles(1:3)
#' }
#' \dontrun{
#' plot_density(results, variables = "Petal.Length")
#' }
#' \dontrun{
#' plot_density(results, bw = TRUE)
#' }
#' \dontrun{
#' plot_density(results, bw = FALSE, conditional = TRUE)
#' }
#' \dontrun{
#' plot_density(results[[2]], variables = "Petal.Length")
#' }
plot_density <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL) {
        UseMethod("plot_density", x)
    }


#' @export
plot_density.default <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL) {

        plot_df <- x
        # Plot figure
        if (bw) {
            if (conditional) {
                plot_df <- plot_df[-which(plot_df$Class == "Total"),]
                density_plot <-
                    ggplot(plot_df,
                           aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
                    geom_density(position = "fill") + scale_fill_grey(start = 0.2, end = 0.8)
            } else{
                density_plot <-
                    ggplot(plot_df,
                           aes_string(x = "Value", linetype = "Class", weight = "Probability")) +
                    geom_density()
            }
        } else{
            if (conditional) {
                plot_df <- plot_df[-which(plot_df$Class == "Total"),]
                density_plot <-
                    ggplot(plot_df,
                           aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
                    geom_density(position = "fill")
            } else{
                density_plot <-
                    ggplot(plot_df,
                           aes_string(x = "Value",
                                      fill = "Class",
                                      colour = "Class",
                                      weight = "Probability"
                           )) +
                    geom_density(alpha = alpha)
            }
        }
        # Relabel facets
        label_facets <- c(levels(plot_df$Variable), levels(plot_df$Title))
        names(label_facets) <- label_facets
        if(!is.null(facet_labels)){
            label_facets[which(tolower(names(label_facets)) %in% tolower(names(facet_labels)))] <- facet_labels[which(tolower(names(facet_labels)) %in% tolower(names(label_facets)))]
        }
        # Facet the plot

        if (length(unique(plot_df$Title)) > 1) {
            if (length(variables) > 1) {

                density_plot <- density_plot +
                    facet_grid(Title ~ Variable, labeller = labeller(Title = label_facets, Variable = label_facets), scales = "free_x")

            } else {
                density_plot <- density_plot +
                    facet_grid( ~ Title, labeller = labeller(Title = label_facets))
            }
        } else {
            if (length(variables) > 1) {
                density_plot <- density_plot +
                    facet_grid( ~ Variable, labeller = labeller(Variable = label_facets))
            }
        }

        density_plot <- density_plot +
            theme_bw() +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0))

        suppressWarnings(print(density_plot))
        return(invisible(density_plot))
    }


#' @export
plot_density.tidyLPA <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL) {
        Args <- as.list(match.call()[-1])

        # Check if all variables (except CPROBs) are identical across models
        plot_df <- lapply(x, function(x)
            as.data.frame(x$dff))

        var_names <-
            names(plot_df[[1]])[-grep("^(model_number|classes_number|CPROB\\d+|Class)$",
                                      names(plot_df[[1]]))]

        # If no variables have been specified, use all variables
        if (is.null(variables)) {
            variables <- var_names
        } else {
            variables <- variables[which((variables) %in% (var_names))]
        }
        if (!length(variables))
            stop("No valid variables provided.")

        plot_df <-
            lapply(plot_df, function(x) {
                x[, which(names(x) %in% c(grep("^CPROB", names(x), value = TRUE), variables))]
            })
        plot_df <- lapply(plot_df, function(x) {
            if (length(grep("^CPROB", names(x))) == 1) {
                names(x) <- gsub("^CPROB1", "Probability.Total", names(x))
                x
            } else {
                names(x) <- gsub("^CPROB", "Probability.", names(x))
                data.frame(x, Probability.Total = 1)
            }

        })

        for (i in names(plot_df)) {
            plot_df[[i]][, grep("^Probability", names(plot_df[[i]]))] <-
                lapply(plot_df[[i]][grep("^Probability", names(plot_df[[i]]))], function(x) {
                    x / length(x)
                })
        }


        plot_df <- lapply(plot_df, function(x) {
            reshape(
                x,
                direction = "long",
                varying =
                    grep("^Probability", names(x), value = TRUE),
                timevar = "Class",
                idvar = "ID"
            )
        })

        plot_df <-
            do.call(rbind, lapply(names(plot_df), function(x) {
                data.frame(Title = gsub("_", " ", x), plot_df[[x]])
            }))

        variable_names <-
            which(!(
                names(plot_df) %in% c("Title", "Class", "Probability", "ID")
            ))

        names(plot_df)[variable_names] <-
            sapply(names(plot_df)[variable_names], function(x) {
                paste(c(
                    "Value_____",
                    toupper(substring(x, 1, 1)),
                    tolower(substring(x, 2))
                ), collapse = "")
            })

        plot_df <- reshape(
            plot_df,
            direction = "long",
            varying =
                grep("^Value", names(plot_df), value = TRUE),
            sep = "_____",
            timevar = "Variable"
        )[, c("Title", "Variable", "Value", "Class", "Probability")]

        plot_df$Variable <- factor(plot_df$Variable)
        plot_df$Class <- factor(plot_df$Class)
        plot_df$Class <-
            ordered(plot_df$Class, levels = c("Total", levels(plot_df$Class)[-length(levels(plot_df$Class))]))
        Args[["variables"]] <- variables
        Args[["x"]] <- plot_df
        do.call("plot_density", Args)
    }
