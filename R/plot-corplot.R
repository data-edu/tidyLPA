if(FALSE){
    extract_rawdata <- function(x, select_vars, wide = TRUE){
        df_raw <- tidyLPA:::get_long_data(x)

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
        df_cors <- est[est$Category == "Covariances", -match("p", names(est))]
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
        #names(df_cors)[match(c("Estimate", "Estimate.Means", "se.Means", "Estimate.Variances", "se.Variances", "Estimate.Means.y", "se.Means.y", "Estimate.Variances.y", "se.Variances.y"), names(df_cors))] <- c("Correlation", "xmean", "xmean_se", "xsd", "xv_se", "ymean", "ymean_se", "ysd", "yv_se")
        df_cors$Correlation <- df_cors$Correlation / (df_cors$xsd*df_cors$ysd)
        df_cors[, c("Parameter", "xvar", "yvar", "Class", "Model", "Classes", "Correlation", "xmean", "ymean", "xsd", "ysd")]
    }


    #' @rdname plot_corplot
    #' @export
    plot_corplot.tidyProfile <- function(x, variables = NULL, ci = .95, sd = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
        Args <- as.list(match.call()[-1])
         #x <- res$model_6_class_2

        df_plot <- get_cordat(as.data.frame(get_estimates(x)))
        df_plot$Class <- ordered(df_plot$Class)
#df_plot$Correlation[1:2] <- c(0,-.5)
        rawdata = TRUE
        if (rawdata) {
            df_raw <- extract_rawdata(x, select_vars = unique(c(df_plot$xvar, df_plot$yvar)))
            df_raw$Class <- ordered(df_raw$Class, levels = levels(df_plot$Class))
        }

        bw = FALSE
        # Basic plot
        p <- base_plot(ifelse(bw, 0, max(df_plot$Classes)))

        sd = TRUE
        cors = TRUE


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

        if (output == "list") return(cor_plotlist)
        merge_corplots(plots)
    }

}

base_plot <- function(num_colors) {
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
        ) + theme_bw()
}


get_palette <- function(x){
    switch(max(x-2, 1),
              rgb(c(228,55,77),
                  c(26,126,175),
                  c(28,184,74),maxColorValue=255),
              rgb(c(228,55,77,152),
                  c(26,126,175,78),
                  c(28,184,74,163),maxColorValue=255),
              rgb(c(228,55,77,152,255),
                  c(26,126,175,78,127),
                  c(28,184,74,163,0),maxColorValue=255),
              rgb(c(228,55,77,152,255,255),
                  c(26,126,175,78,127,255),
                  c(28,184,74,163,0,51),maxColorValue=255),
              rgb(c(228,55,77,152,255,255,166),
                  c(26,126,175,78,127,255,86),
                  c(28,184,74,163,0,51,40),maxColorValue=255),
              rgb(c(228,55,77,152,255,255,166,247),
                  c(26,126,175,78,127,255,86,129),
                  c(28,184,74,163,0,51,40,191),maxColorValue=255),
              rgb(c(228,55,77,152,255,255,166,247,153),
                  c(26,126,175,78,127,255,86,129,153),
                  c(28,184,74,163,0,51,40,191,153),maxColorValue=255)
    )[1:x]
}

#' @import grid gtable
merge_corplots <- function(plots, ...){
    args <- match.call()
    if(!("ylab" %in% names(args))){
        ylab <- plots[[1]]$labels$y
    }
    plots <- cor_plotlist
    n_vars <- ((sqrt(8*(length(plots)) + 1) - 1)/2)+1

    grob_list <- lapply(1:(n_vars*n_vars), nullGrob)

    model_mat <- matrix(1L:(n_vars*n_vars), nrow = n_vars)
    grob_cor <- model_mat[lower.tri(model_mat)]
    grob_list[grob_cor] <- plots

    grob_dens <- diag(model_mat)
    grob_null <- grob_cor <- model_mat[upper.tri(model_mat)]

    grob_cols <- grob_rows <- n_vars

    no_x <- as.vector(model_mat[-nrow(model_mat), ])
    no_x <- no_x[!no_x %in% grob_null]
    no_y <- model_mat[nrow(model_mat), -1]
    no_x_y <- model_mat[lower.tri(model_mat, diag = TRUE)]
    no_x_y <- no_x_y[!no_x_y %in% c(has_x, has_y)]

#     This is to remove legends and axis and adjust width
    grob_list[[n_vars]] <- suppressMessages(ggplotGrob(grob_list[[n_vars]]))
    fixed_widths <- grob_list[[n_vars]]$widths
    grob_list[no_x] <- lapply(grob_list[no_x], function(this_plot){
        if(inherits(this_plot, "ggplot")){
            suppressMessages(ggplotGrob(this_plot+theme(axis.title.x = element_blank(),
                                                        axis.text.x = element_blank(),
                                                        axis.ticks.x = element_blank())))
        }
        })
    grob_list[no_y] <- lapply(grob_list[no_y], function(this_plot){
        if(inherits(this_plot, "ggplot")){
        suppressMessages(ggplotGrob(this_plot+theme(axis.title.y = element_blank(),
                                                    axis.text.y = element_blank(),
                                                    axis.ticks.y = element_blank())))
        }
    })
    grob_list[no_x_y] <- lapply(grob_list[no_x_y], function(this_plot){
        if(inherits(this_plot, "ggplot")){
        suppressMessages(ggplotGrob(this_plot+theme(axis.title.x = element_blank(),
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    axis.text.y = element_blank(),
                                                    axis.ticks.y = element_blank())))
        }
    })
    grob_list[c(no_x, no_y, no_x_y)] <- lapply(grob_list[c(no_x, no_y, no_x_y)], function(x){
        x$widths <- fixed_widths
        x
    })

    grob_list[grob_dens] <- lapply(1:n_vars, nullGrob)
    #
    # for(x in 1:length(plots)){
    #     if(x %in%)
    #     if(!(x %in% model_mat[-nrow(model_mat), -1][lower.tri(model_mat[-nrow(model_mat), -1])]){
    #         plots[[x]] <- plots[[x]] + theme(axis.text.y = element_blank(),
    #                                          axis.ticks.y = element_blank())
    #     }
    #     if(!x == grob_cols){
    #         plots[[x]] <- plots[[x]] +
    #     }
    #     plots[[x]] <- suppressMessages(ggplotGrob(plots[[x]]+theme(axis.title.y = element_blank())))
    #     if(x > 1) plots[[x]]$widths <- plots[[1]]$widths
    # }

    gt <- gtable_matrix("corr_plot",
                        matrix(grob_list, nrow = n_vars, ncol = n_vars),
                        widths = unit(rep(1, n_vars), "null"),
                        heights = unit(rep(1, n_vars), "null"))

    left <- textGrob(ylab, rot = 90, just = c(.5, .5))
    gt <- gtable_add_cols(gt, widths = grobWidth(left)+ unit(0.5, "line"), 0)
    gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt),
                          l = 1, r = 1, z = Inf)
    gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))

    grid.newpage()
    grid.draw(gt)
    invisible(gt)
}

