test_that("plot_profiles works", {
  x <- estimate_profiles(iris[, 1:4], n_profiles = 3)
  expect_s3_class(plot_profiles(x), "ggplot")
})

pisa_profiles_plot_mclust <- pisaUSA15[1:100, ] %>%
    single_imputation() %>%
    scale() %>%
    estimate_profiles(3) %>%
    plot_profiles()

test_that("plot profiles works with imputed and scaled data using mclust", {
    expect_s3_class(pisa_profiles_plot_mclust, "ggplot")
})
