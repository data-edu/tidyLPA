.extract_rawdata <- function(x, select_vars, wide = TRUE){
    df_raw <- .get_long_data(list(x))

    df_raw <- df_raw[, c("model_number", "classes_number", select_vars, "Class", "Class_prob", "Probability", "id")]
    if(!wide){
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
    }

    if(any(c("Class_prob", "id", "new_id") %in% names(df_raw))){
        df_raw <- df_raw[, -which(names(df_raw) %in% c("Class_prob", "id", "new_id"))]
    }
    names(df_raw)[c(1,2)] <- c("Model", "Classes")
    df_raw
}


make_ellipsis <- function(r, xmean, ymean, sdx, sdy){
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = 20)
    matrix(c(sdx * cos(a + d/2) + xmean, sdy * cos(a - d/2) + ymean), 20, 2, dimnames = list(NULL, c("x", "y")))
}

get_cordat <- function(est){
    df_cors <- est[est$Category == "Covariances", -match("p", names(est)), drop = FALSE]
    if(nrow(df_cors) == 0){
        vars <- est[est$Class == 1 & est$Category == "Means", ]$Parameter
        cors <- apply(expand.grid(vars, vars), 1, paste0, collapse = ".WITH.")[which(upper.tri(diag(vars)))]
        df_cors <- data.frame(Category = "Covariances",
                              Parameter = cors,
                              Estimate = 0,
                              se = 0,
                              Class = rep(unique(est$Class), each = length(cors)),
                              Model = est$Model[1],
                              Classes = est$Classes[1])

    }
    df_cors$xvar <- gsub("\\.WITH.*$", "", df_cors$Parameter)
    df_cors$yvar <- gsub("^.+?WITH\\.", "", df_cors$Parameter)
    df_cors$id <- do.call(paste0, df_cors[c("Class", "Model", "Classes")])
    est <- est[!est$Category == "Covariances", -match("p", names(est))]
    est$id <- do.call(paste0, est[c("Parameter", "Class", "Model", "Classes")])
    est <- reshape(est, direction = "wide", v.names = c("Estimate", "se"), timevar = "Category", idvar = "id")
    #est$id <- do.call(paste0, est[c("Class", "Model", "Classes")])
    df_cors$idx <- do.call(paste0, df_cors[c("xvar", "Class", "Model", "Classes")])
    df_cors$idy <- do.call(paste0, df_cors[c("yvar", "Class", "Model", "Classes")])




    df_cors <- cbind(df_cors,
                     est[match(df_cors$idx, est$id), c("Estimate.Means", "Estimate.Variances")],
                     est[match(df_cors$idy, est$id), c("Estimate.Means", "Estimate.Variances")])

    #df_cors <- merge(df_cors, est[est$Parameter == df_cors$xvar, ], by = "id", suffixes = c("", ".x"))
    #df_cors <- merge(df_cors, est[est$Parameter == df_cors$yvar, ], by = "id", suffixes = c("", ".y"))
    names(df_cors)[c(3, 13:16)] <- c("Correlation", "xmean", "xsd", "ymean", "ysd")
    df_cors <- df_cors[, -which(names(df_cors) %in% c("id", "idx", "idy", "Category", "se", "Parameter.x", "Class.x", "Model.x", "Classes.x",
                                                      "Parameter.y", "Class.y", "Model.y", "Classes.y"))]
    df_cors$xsd <- sqrt(df_cors$xsd)
    df_cors$ysd <- sqrt(df_cors$ysd)
    df_cors$Correlation <- df_cors$Correlation / (df_cors$xsd*df_cors$ysd)
    df_cors[, c("Parameter", "xvar", "yvar", "Class", "Model", "Classes", "Correlation", "xmean", "ymean", "xsd", "ysd")]
}

# Create correlation plots for a mixture model
#
# Creates a faceted plot of two-dimensional correlation plots and
# unidimensional density plots for an object of class 'tidyProfile'.
# @param x tidyProfile object to plot. A tidyProfile is one element of a
# tidyLPA analysis.
# @param variables Which variables to plot. If NULL, plots all variables that
# are present in all models.
# @param sd Logical. Whether to show the estimated standard deviations as lines
# emanating from the cluster centroid.
# @param cors Logical. Whether to show the estimated correlation (standardized
# covariance) as ellipses surrounding the cluster centroid.
# @param rawdata Logical. Whether to plot raw data, weighted by posterior class
# probability.
# @param bw Logical. Whether to make a black and white plot (for print) or a
# color plot. Defaults to FALSE, because these density plots are hard to read
# in black and white.
# @param alpha_range Numeric vector (0-1). Sets
# the transparency of geom_density and geom_point.
# @param return_list Logical. Whether to return a list of ggplot objects, or
# just the final plot. Defaults to FALSE.
# @return An object of class 'ggplot'.
# @author Caspar J. van Lissa
# @export
# @examples
# # Example 1
# iris_sample <- iris[c(1:10, 51:60, 101:110), ] # to make example run more quickly
# \dontrun{
# iris_sample %>%
#  subset(select = c("Sepal.Length", "Sepal.Width")) %>%
#  estimate_profiles(n_profiles = 2, models = 1) %>%
#  plot_bivariate()
#}
# # Example 2
# \dontrun{
# mtcars %>%
#   subset(select = c("wt", "qsec", "drat")) %>%
#   poms() %>%
#   estimate_profiles(3) %>%
#   plot_bivariate()
#}
# @keywords mixture correlation plot
# @rdname plot_bivariate
# @export
# plot_bivariate <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE){
#    UseMethod("plot_bivariate", x)
# }

#' @importFrom tidySEM plot_bivariate
#' @export
tidySEM::plot_bivariate


#' @method plot_bivariate tidyLPA
#' @export
plot_bivariate.tidyLPA <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE, ...){
    Args <- match.call()
    if(length(x) == 1){
        Args$x <- x[[1]]
        Args[[1]] <- as.name("plot_bivariate")
        eval.parent(Args)
    } else {
        stop("plot_bivariate can only plot a single tidyProfile object. This tidyLPA object contains ", length(x), " tidyProfile objects. Extract one of these objects using '$' or '[[]]' and try again.")
    }
}


#' @method plot_bivariate tidyProfile
#' @export
plot_bivariate.tidyProfile <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE, ...){
    est <- get_estimates(x)
    if(is.null(est)){
        stop("Cannot plot this tidyProfile, because it does not contain estimates. Check whether this model has converged.", call. = FALSE)
    }
    df_plot <- get_cordat(as.data.frame(est))

    df_plot$Class <- ordered(df_plot$Class)

    if (rawdata) {
        df_raw <- .extract_rawdata(x, select_vars = unique(c(df_plot$xvar, df_plot$yvar)))
        df_raw$Class <- ordered(df_raw$Class, levels = levels(df_plot$Class))
    }
    # Basic plot
    p <- .base_plot(ifelse(bw, 0, max(df_plot$Classes)))
    Args <- list(
       x = list("model" = x),
       variables = unique(c(df_plot$xvar, df_plot$yvar)),
       longform = FALSE
    )
    df_density <- do.call(.extract_density_data, Args)

    # n <- nrow(x$dff)
    # df_density2 <- df_raw[1:n, ]
    # df_density2$Class <- "Total"
    # df_density2$Probability <- 1
    # df_density2 <- rbind(df_raw, df_density2)
    # df_density2$Probability <- df_density2$Probability / n
    # df_density2$Class <- ordered(df_density2$Class, levels = c(tail(levels(df_density2$Class), 1), levels(df_density2$Class)[-length(levels(df_density2$Class))]))

    args_dens <- list(plot_df = df_density,
                      variables = NULL)

    dens_plotlist <- lapply(Args$variables, function(thisvar){
        names(args_dens$plot_df)[which(names(args_dens$plot_df) == thisvar)] <- "Value"
        args_dens$variables <- thisvar
        do.call(.plot_density_fun, args_dens) + theme_bw() + labs(x = thisvar, y = thisvar)
    })

    cor_plotlist <- lapply(unique(df_plot$Parameter), function(this_cor){
        df_plot <- df_plot[df_plot$Parameter == this_cor, ]
        p <- p + geom_point(data = df_plot, aes_string(x = "xmean", y = "ymean"))
        if(sd){
            df_sd <- df_plot
            df_sd$sdminx <- df_sd$xmean - df_sd$xsd
            df_sd$sdmaxx <- df_sd$xmean + df_sd$xsd
            df_sd$sdminy <- df_sd$ymean - df_sd$ysd
            df_sd$sdmaxy <- df_sd$ymean + df_sd$ysd
            p <- p +
                geom_errorbar(data = df_sd, aes_string(
                    x = "xmean",
                    ymin = "sdminy",
                    ymax = "sdmaxy"),
                    width = .0) +
                geom_errorbarh(data = df_sd, aes_string(
                    y = "ymean",
                    xmin = "sdminx",
                    xmax = "sdmaxx"),
                    height = .0)
        }
        if(cors){
            # Make data.frame for elipses
            df_ellipse <- do.call(rbind, apply(df_plot, 1, function(x) {
                data.frame(do.call(make_ellipsis,
                                   as.list(as.numeric(x[c(7:11)]))),
                           t(x[c(1:6)]))
            }))
            p <- p + geom_path(data = df_ellipse, aes_string(x = "x",
                                                             y = "y"))
        }
        if (rawdata) {
            p <- p +
                geom_point(
                    data = df_raw,
                    aes_string(
                        x = df_plot$xvar[1],
                        y = df_plot$yvar[1],
                        alpha = "Probability"
                    )
                ) +
                scale_alpha_continuous(range = alpha_range, guide = FALSE)
        }
        p + labs(x = df_plot$xvar[1], y = df_plot$yvar[1])
    })
    n_vars <- length(dens_plotlist)
    model_mat <- matrix(1L:(n_vars*n_vars), nrow = n_vars)
    plot_list <- vector("list", length = length(model_mat))
    plot_list[diag(model_mat)] <- dens_plotlist
    plot_list[which(lower.tri(model_mat))] <- cor_plotlist
    if (return_list) return(plot_list)
    merge_corplots(plot_list)
}



.base_plot <- function(num_colors) {
    p <- ggplot(NULL,
               aes_string(
                   group = "Class",
                   linetype = "Class",
                   shape = "Class"
               ))
    if(num_colors > 0){
        p <- p + aes_string(colour = "Class") +
            scale_colour_manual(values = get_palette(num_colors))
    }
    p + theme(
            legend.direction = "vertical",
            legend.box = "horizontal",
            legend.position = c(1, .997),
            legend.justification = c(1, 1)
        ) + theme_bw() +
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))

}


get_palette <- function(x){
    if(x < 10){
        switch(max(x-2, 1),
               c("#E41A1C", "#377EB8", "#4DAF4A"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
               c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
        )[1:x]
    } else {
        colrs <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
        c(get_palette(9), sample(colrs, (x-9)))
    }
}

#' @import grid gtable
#' @importFrom stats na.omit
merge_corplots <- function(plots, ...) {
    suppressWarnings({
        suppressMessages({

            n_vars <- sqrt(length(plots))

            null_grobs <- sapply(plots, inherits, what = "NULL")
            plots[null_grobs] <- lapply(1:sum(null_grobs), nullGrob)

            plot2_grobs <- ggplot_gtable(ggplot_build(plots[[2]]))
            grob_legend <-
                plot2_grobs$grobs[[which(sapply(plot2_grobs$grobs, `[[`, "name") == "guide-box")]]
            width_grob <- grobWidth(plot2_grobs$grobs[[grep("^axis.title.y.left", sapply(plot2_grobs$grobs, `[[`, "name"))]])

            # axes <- lapply(plots[1:n_vars], function(x){
            #     tmp <- ggplot_gtable(ggplot_build(x))
            #     tmp$grobs[[grep("^axis.title.y.left", sapply(tmp$grobs, `[[`, "name"))]]
            #     })

            model_mat <- matrix(1L:(n_vars * n_vars), nrow = n_vars)
            model_mat[upper.tri(model_mat)] <- NA

            no_x_y <- na.omit(as.vector(model_mat[-nrow(model_mat),-1]))
            keep_x <- model_mat[nrow(model_mat),-1, drop = TRUE]
            keep_y <- model_mat[-nrow(model_mat), 1, drop = TRUE]

            #     This is to remove legends and axis and adjust width
            plots[[n_vars]] <-
                ggplotGrob(plots[[n_vars]] + theme(legend.position = "none"))
            fixed_widths <- plots[[n_vars]]$widths
            fixed_heights <- plots[[n_vars]]$heights

            plots[keep_y] <- lapply(plots[keep_y], function(this_plot) {
                if (inherits(this_plot, "ggplot")) {
                    ggplotGrob(
                        this_plot + theme(
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            legend.position = "none"
                        )
                    )
                }
            })
            plots[keep_x] <- lapply(plots[keep_x], function(this_plot) {
                if (inherits(this_plot, "ggplot")) {
                    ggplotGrob(
                        this_plot + theme(
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            legend.position = "none"
                        )
                    )
                }
            })
            plots[no_x_y] <- lapply(plots[no_x_y], function(this_plot) {
                if (inherits(this_plot, "ggplot")) {
                    ggplotGrob(
                        this_plot + theme(
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            legend.position = "none"
                        )
                    )
                }
            })

            for(x in 1:length(plots)){
                plots[[x]]$widths <- fixed_widths
                if(x > n_vars){
                    plots[[x]]$widths[c(1,3)] <- unit(0, "cm")
                    plots[[x]]$widths[4] <- plots[[x]]$widths[4]+width_grob
                }
                plots[[x]]$heights <- fixed_heights
                if(!x %in% model_mat[nrow(model_mat), ]){
                    plots[[x]]$heights[c(1,9)] <- unit(0, "cm")
                    plots[[x]]$heights[8] <- plots[[x]]$heights[8]+width_grob
                }
            }
            #plots[-c(1:n_vars)] <- lapply(plots[-c(1:n_vars)], function(x) {


                #x$heights <- fixed_heights
            #    x
            #})

            plots[[((n_vars - 1) * n_vars) + 1]] <- grob_legend

            gt <- gtable_matrix(
                "corr_plot",
                matrix(plots, nrow = n_vars, ncol = n_vars),
                widths = unit(rep(1, n_vars), "null"),
                heights = unit(rep(1, n_vars), "null")
            )


            #left <- textGrob(ylab, rot = 90, just = c(.5, .5))
            #gt <- gtable_add_cols(gt, widths = grobWidth(axes[[1]])+ unit(0.5, "line"), 0)
            #gt <- gtable_add_grob(gt, axes, t = 1, b = nrow(gt),
            #                       l = 1, r = 1, z = Inf)
            # gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))

            grid.newpage()
            grid.draw(gt)
            invisible(gt)
        })
    })
}
