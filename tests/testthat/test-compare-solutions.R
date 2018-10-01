context("test-compare_solutions.R")

test_that("compare_solutions works", {
  x <- compare_solutions(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  expect_s3_class(x, "ggplot")
})
