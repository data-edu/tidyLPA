context("test-compare_solutions.R")

test_that("compare_solutions works", {
    x <- estimate_profiles(iris[, 1:4], 1:3)
    expect_s3_class(compare_solutions(x), "bestLPA")
})
