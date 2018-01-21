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
