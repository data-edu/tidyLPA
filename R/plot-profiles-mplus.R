#' Plot variable means and their confidence intervals by profile for models estimated with MPlus (requires purchasing and installing MPlus to use)
#' @details Plot the variable means and variances for data frame output from estimate_profiles_mclust()
#' @param mplus_data output from estimate_profiles_mplus() with return_savedata = T specified
#' @param plot_post_probs logical; whether to plot the means and standard errors based on the posterior probabilities (if TRUE) or on the classification/profile (if FALSE); defaults to TRUE
#' @param mplus_out_name character string; name of the mplus out file that is read if the posterior probabilities are plotted; defaults to i.out (which is the default name for the file created by other tidyLPA functions)
#' @param standard_error_interval number between 0 and 1; defaults to .95
#' @inheritParams plot_profiles
#' @examples
#' \dontrun{
#'
#' m <- estimate_profiles_mplus(iris,
#' Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#' n_profiles = 2)
#'
#' plot_profiles_mplus(m)
#'
#' m <- estimate_profiles_mplus(iris,
#' Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
#' n_profiles = 2, latent_vars = list(sepal = c(1, 2), petal = c(3, 4)),
#' remove_tmp_files = FALSE)
#'
#' plot_profiles_mplus(mplus_out_name = "i.out")
#' }
#' @export

plot_profiles_mplus <- function(mplus_data = NULL,
                                to_center = FALSE,
                                to_scale = FALSE,
                                plot_post_probs = FALSE,
                                mplus_out_name = NULL,
                                standard_error_interval = .95) {

    if (!is.null(mplus_out_name)) {

        m <- suppressWarnings(MplusAutomation::readModels(mplus_out_name)) # for model reading warnings

        if (m$summaries$NContinuousLatentVars > 0) {

            d <- m %>% purrr::pluck("parameters", "unstandardized")
            d$param <- ifelse(nchar(d$param) > 8, str_sub(d$param, end = 8), d$param)

            if (to_center == TRUE) {
                the_means <-  m$sampstat$means %>%
                    dplyr::as_data_frame() %>%
                    tidyr::gather("key", "intercept_mean") %>%
                    dplyr::rename(param = .data$key) %>%
                    dplyr::mutate(paramHeader = "Intercepts")

                d <- dplyr::left_join(d, the_means, by = c("paramHeader", "param"))
                d$est <- ifelse(d$paramHeader == "Intercepts", d$est - d$intercept_mean, d$est)
            }

            overall_factor_loadings <- d %>%
                dplyr::filter(str_detect(.data$paramHeader, ".BY")) %>%
                dplyr::filter(.data$LatentClass == 1) %>%
                dplyr::mutate(latent = stringr::str_sub(.data$paramHeader, end = -4)) %>%
                dplyr::select(.data$latent, observed = .data$param, loading = .data$est, .data$se)

            means_zero_scalar <- d %>%
                dplyr::filter(str_detect(.data$paramHeader, "Means")) %>%
                dplyr::filter(.data$est == 0.000) %>%
                dplyr::summarize(lc_means_zero = mean(as.integer(.data$LatentClass))) %>%
                dplyr::pull()

            d_sub <- d %>%
                dplyr::filter(.data$LatentClass == .data$means_zero_scalar)

            one_class_indicators <- d_sub %>%
                dplyr::filter(str_detect(.data$paramHeader, "Intercepts")) %>%
                dplyr::rename(observed = .data$param, intercept = .data$est) %>%
                dplyr::left_join(overall_factor_loadings, by = "observed") %>%
                dplyr::mutate(value = .data$intercept * .data$loading) %>%
                dplyr::select(.data$observed, .data$latent, .data$intercept, .data$loading, .data$value, .data$LatentClass)

            one_class_raw_means <- one_class_indicators %>%
                dplyr::group_by(.data$latent) %>%
                dplyr::summarize(zero_mean_est = mean(.data$value))

            to_plot <- d %>%
                dplyr::filter(.data$LatentClass != "Categorical.Latent.Variables") %>%
                dplyr::filter(.data$paramHeader == "Means") %>%
                dplyr::filter(.data$param != "C#1") %>%
                dplyr::select(latent = .data$param, .data$est, intercept_se = .data$se, .data$LatentClass) %>%
                dplyr::left_join(one_class_raw_means, by = "latent") %>%
                dplyr::mutate(adjusted_est = .data$est + .data$zero_mean_est) %>%
                dplyr::select(.data$latent, est = .data$adjusted_est, class = .data$LatentClass)

            p <- ggplot2::ggplot(to_plot, ggplot2::aes(x = .data$class,
                                                       y = .data$est,
                                                       fill = .data$latent,
                                                       group = .data$latent)) +
                 ggplot2::geom_col(position = "dodge") +
                 ggplot2::theme_bw() +
                 ggplot2::scale_x_discrete(NULL) +
                 ggplot2::scale_fill_discrete(NULL) +
                 ggplot2::ylab("Estimate (raw score)")

            if (to_center == TRUE) {
                p + ylab("Centered values")
            }

            return(p)

        }
    }

    if (plot_post_probs == TRUE) {

        x <- suppressWarnings(MplusAutomation::readModels(mplus_out_name))
        number_of_rows <- nrow(x$savedata)
        se_diff <- standard_error_interval + ((1 - standard_error_interval) / 2)
        z <- count(x$savedata, .data$C)
        profile_names <- paste0("Profile ", z$C, " (n = ", z$n, ")")

        p <- seq(x$tech7) %>%
            map_df(extract_prob_stats, obj = x$tech7) %>%
            ggplot(aes(x = as.character(class),
                       y = .data$class_mean,
                       ymin = .data$class_mean - (1.96 * ((.data$class_sd / sqrt(number_of_rows)))),
                       ymax = .data$class_mean + (1.96 * ((.data$class_sd / sqrt(number_of_rows)))),
                       fill = .data$var
            )) +
            geom_col(position = "dodge") +
            geom_errorbar(position = "dodge") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_x_discrete("", labels = profile_names) +
            ylab("Mean")

        return(p)

    }

    mplus_data <- mplus_data[, -ncol(mplus_data)]

    z <- count(mplus_data, .data$C)

    d <- mplus_data %>%
        left_join(z, by = "C") %>%
        mutate(profile = paste0("Profile ", .data$C, " (n = ", .data$n, ")")) %>%
        select(-contains("CPROB"), -.data$C, -.data$n) %>%
        mutate_at(vars(-.data$profile),
                  scale,
                  center = to_center,
                  scale = to_scale
        ) %>%
        dplyr::slice(1:nrow(.)) %>% # this is to overcome a weird bug in the subsequent gather function, which outputs a warning
        group_by(.data$profile) %>%
        summarize_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE))) %>%
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
            se = 1.96 * (.data$sd / sqrt(.data$n - 1)),
            ymin = .data$mean - .data$se,
            ymax = .data$mean + .data$se
        )

    p <- ggplot(d, aes_string(
        x = "profile",
        y = "mean",
        fill = "key",
        ymin = "ymin",
        ymax = "ymax"
    )) +
        geom_col(position = "dodge") +
        geom_errorbar(position = position_dodge()) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete("")

    p
}
