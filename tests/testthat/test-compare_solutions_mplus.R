context("test-compare_models_mplus.R")

test_that("compare_solutions_mplus() works", {
  skip_on_cran()
  skip_on_travis()
  x <- compare_solutions_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, n_profiles_max = 2)
  expect_s3_class(x[[2]], "data.frame")
})
