context("test-plot_profiles.R")

test_that("plot_profiles works", {
  x <- estimate_profiles(iris[, 1:4], n_profiles = 3)
  expect_s3_class(plot_profiles(x), "ggplot")
})
