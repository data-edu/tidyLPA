context("test-plot_profiles_mplus.R")

test_that("plot_profiles", {
  skip_on_cran()
  skip_on_travis()
  x <- suppressWarnings(estimate_profiles(iris[, 1:4], n_profiles = 3, package = "MplusAutomation"))
  expect_s3_class(plot_profiles(x), "ggplot")
})
