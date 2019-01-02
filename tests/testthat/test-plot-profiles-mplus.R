context("test-plot_profiles_mplus.R")

test_that("plot_profiles", {
  skip_on_cran()
  skip_on_travis()
  out_mplus <- suppressWarnings(estimate_profiles(iris[, 1:4], n_profiles = 3, package = "MplusAutomation"))
  #out_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3)
  expect_s3_class(plot_profiles(out_mplus), "ggplot")

})
