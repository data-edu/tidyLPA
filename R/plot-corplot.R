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
        est$id <- do.call(paste0, est[c("Class", "Model", "Classes")])

        df_cors <- merge(df_cors, est[est$Parameter == df_cors$xvar, ], by = "id", suffixes = c("", ".x"))
        df_cors <- merge(df_cors, est[est$Parameter == df_cors$yvar, ], by = "id", suffixes = c("", ".y"))
        df_cors <- df_cors[, -which(names(df_cors) %in% c("id", "Category", "se", "Parameter.x", "Class.x", "Model.x", "Classes.x",
                                                          "Parameter.y", "Class.y", "Model.y", "Classes.y"))]
        df_cors$Estimate.Variances <- sqrt(df_cors$Estimate.Variances)
        df_cors$Estimate.Variances.y <- sqrt(df_cors$Estimate.Variances.y)
        names(df_cors)[match(c("Estimate", "Estimate.Means", "se.Means", "Estimate.Variances", "se.Variances", "Estimate.Means.y", "se.Means.y", "Estimate.Variances.y", "se.Variances.y"), names(df_cors))] <- c("Correlation", "xmean", "xmean_se", "xsd", "xv_se", "ymean", "ymean_se", "ysd", "yv_se")
        df_cors$Correlation <- df_cors$Correlation / (df_cors$xsd*df_cors$ysd)
        df_cors
    }


    #' @rdname plot_corplot
    #' @export
    plot_corplot.tidyProfile <- function(x, variables = NULL, ci = .95, sd = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
        Args <- as.list(match.call()[-1])
         #x <- res$model_6_class_2

        df_plot <- get_cordat(as.data.frame(get_estimates(x)))
        # ID to apply over:
        #df_plot$plotid <- do.call(paste0, df_plot[c("Model", "Classes", "xvar", "yvar")])

        df_plot$Class <- ordered(df_plot$Class)
        # Basic plot
        if (bw) {
            p <-
                ggplot(NULL,
                       aes_string(
                           group = "Class",
                           linetype = "Class",
                           shape = "Class"
                       ))
        } else {
            p <-
                ggplot(
                    NULL,
                    aes_string(
                        group = "Class",
                        linetype = "Class",
                        shape = "Class",
                        colour = "Class"
                    )
                )
        }

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
                                   as.list(as.numeric(x[c(1, 7, 11, 9, 13)]))),
                           t(x[c(2:6)]))
            }))
            p <- p + geom_path(data = df_ellipse, aes_string(x = "x",
                                                             y = "y"))
        }
        if (rawdata) {
            df_raw <- extract_rawdata(x, select_vars = unique(c(df_plot$xvar, df_plot$yvar)))
            df_raw$Class <- ordered(df_raw$Class, levels = levels(df_plot$Class))
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
        p <- p + labs(x = df_plot$xvar[1], y = df_plot$yvar[1]) +
            theme_bw() +
            theme(
                legend.direction = "vertical",
                legend.box = "horizontal",
                legend.position = c(1, .997),
                legend.justification = c(1, 1)
            )

    }

}
