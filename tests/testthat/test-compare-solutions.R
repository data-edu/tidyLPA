context("test-compare_solutions.R")

x <- estimate_profiles(iris[, 1:4], 1:3)

test_that("compare_solutions works with iris data when we use compare_solutions()
          outside of a pipe", {
    expect_s3_class(compare_solutions(x), "bestLPA")
})
