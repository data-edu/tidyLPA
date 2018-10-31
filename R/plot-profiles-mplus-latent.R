library(MplusAutomation)
library(tidyverse)

here::set_here("~/Desktop/compare_solutions_mplus_output-2018-10-23")

f <- "~/Desktop/compare_solutions_mplus_output-2018-10-30/m-1_p-2/i.out"
x <- MplusAutomation::readModels(f)

plot_profiles_mplus_latent <- function(mplus_output) {

    if (x$summaries$NContinuousLatentVars > 0) {

        d <- mplus_output %>%
            pluck("unstandardized") %>%
            tbl_df()

        overall_factor_loadings <- d %>%
            filter(str_detect(paramHeader, ".BY")) %>%
            filter(LatentClass == 1) %>%
            mutate(latent = str_sub(paramHeader, end = -4)) %>%
            select(latent, observed = param, loading = est)

        means_zero_scalar <- d %>%
            filter(str_detect(paramHeader, "Means")) %>%
            filter(est == 0.000) %>%
            summarize(lc_means_zero = mean(as.integer(LatentClass))) %>%
            pull()

        d_sub <- filter(d, LatentClass == means_zero_scalar) %>% tbl_df()

        one_class_indicators <- d_sub %>%
            filter(str_detect(paramHeader, "Intercepts")) %>%
            rename(observed = param, intercept = est) %>%
            left_join(overall_factor_loadings) %>%
            mutate(value = intercept * loading) %>%
            select(observed, latent, intercept, loading, value, LatentClass)

        one_class_raw_means <- one_class_indicators %>%
            group_by(latent) %>%
            summarize(zero_mean_est = mean(value))

        to_plot <- d %>%
            filter(paramHeader == "Means") %>%
            filter(param != "C#1") %>%
            select(latent = param, est, se, LatentClass) %>%
            left_join(one_class_raw_means, by = "latent") %>%
            mutate(adjusted_est = est + zero_mean_est) %>%
            select(latent, est = adjusted_est, se, class = LatentClass)

        ggplot(to_plot, aes(x = class, y = est, fill = latent, group = latent)) +
            geom_col(position = "dodge") +
            theme_bw() +
            scale_x_discrete(NULL) +
            scale_fill_discrete(NULL) +
            ylab("Estimate (raw score)")

    }
}

plot_profiles_mplus_latent(x$parameters)
