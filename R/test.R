# my_n_processors <- parallel::detectCores()
#
# start_time <- Sys.time()
#
# # m1 <- create_profiles_mplus(iris,
# #                             Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
# #                             n_profiles = 2,
# #                             model = 1,
# #                             n_processors = 1)
#
# compare_models_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, n_profiles_max = 5, start = c(100, 20), n_processors = 1)
#
# end_time <- Sys.time()
# x <- round(start_time - end_time, 2)
# s <- unclass(x[1])[1] * -1
# message(paste0("Took ", s, " seconds ", "(", round(s / 60, 2), " minutes) to run."))

library(tidyverse)

mplus_data <- readr::read_rds("~/desktop.m.rds")

to_center = T
to_scale = T

z <- count(mplus_data[[2]], .data$C)

d <- mplus_data[[2]] %>%
    left_join(z, by = "C") %>%
    mutate(profile = paste0("Profile ", .data$C, " (n = ", .data$n, ")")) %>%
    select(-contains("CPROB"), -.data$C, -.data$n) %>%
    mutate_at(vars(-.data$profile), scale, center = to_center, scale = to_scale) %>%
    group_by(.data$profile) %>%
    summarize_all(funs(mean, sd)) %>%
    gather("key", "val", -.data$profile) %>%
    mutate(new_key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, start = -4),
                            ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, start = -2), NA)),
           key = ifelse(str_sub(.data$key, start = -4) == "mean", str_sub(.data$key, end = -6),
                        ifelse(str_sub(.data$key, start = -2) == "sd", str_sub(.data$key, end = -4), NA))) %>%
    spread(.data$new_key, .data$val) %>%
    mutate(n_string = str_sub(as.character(.data$profile), start = 11),
           n = as.numeric(str_extract(.data$n_string, "\\-*\\d+\\.*\\d*")),
           se = 1.96 * (.data$sd / sqrt(.data$n - 1)),
           ymin = .data$mean - .data$se,
           ymax = .data$mean + .data$se)

d$key <- factor(d$key)
d$key <- fct_recode(d$key,
                    "Affective Engagement" = "DM_AFF_E",
                    "Behavioral Engagement" = "DM_BEH_E",
                    "Challenge" = "DM_CHALL",
                    "Cognitive Engagement" = "DM_COG_E",
                    "Competence" = "DM_COMPE")

d$key <- fct_relevel(d$key, c("Affective Engagement", "Behavioral Engagement", "Cognitive Engagement", "Challenge", "Competence"))

l <- c("Profile 4 (n = 982)",
       "Profile 2 (n = 524)",
       "Profile 6 (n = 259)",
       "Profile 5 (n = 410)",
       "Profile 1 (n = 266)",
       "Profile 3 (n = 517)")

rev(l)

d$profile <- fct_relevel(d$profile,
                         rev(l))

levels(d$profile) <- c("Universally Low (n = 517)",
"Highly Competent but not Engaged (n = 266)",
"Pleasurable (n = 410)",
"Highly Challenged (n = 259)",
"Engaged but not Challenged (n = 524)",
"Full (n = 982)")

p <- ggplot(d, aes_string(x = "profile", y = "mean", fill = "key", ymin = "ymin", ymax = "ymax")) +
    geom_col(position = "dodge") +
    geom_errorbar(position = position_dodge(width = .9)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer("", palette="Set2") +
    ylab("Z-score") +
    theme(text = element_text(size = 13, family = "Helvetica Neue")) +
    theme(legend.position = "none") +
    xlab(NULL) +
    theme(axis.text.x=element_blank())

p + scale_x_discrete(limit = levels(d$profile)[4])
ggsave("2018-1-21-diss-d.png", width = 3, height = 5)

