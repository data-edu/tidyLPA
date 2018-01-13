context("test-compare_models_lpa-r.R")

test_that("compare_models_lpa() works", {
  x <- compare_models_lpa(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  expect_s3_class(x, "ggplot")
})
