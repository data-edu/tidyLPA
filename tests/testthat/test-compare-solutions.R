context("test-compare_solutions.R")

x <- estimate_profiles(iris[, 1:4], n_profiles = 1:3)

test_that("compare_solutions works with iris data when we use compare_solutions()
          outside of a pipe", {
    expect_s3_class(compare_solutions(x), "bestLPA")
})

# pisaUSA_compare <- pisaUSA15[1:100, ] %>%
#     single_imputation() %>%
#     estimate_profiles(2:3,
#                       variances = c("equal", "varying"),
#                       covariances = c("zero", "varying")) %>%
#     compare_solutions(statistics = c("AIC", "BIC"))
#
# test_that("compare_solutions works with PISA data when we use compare_solutions
#           in a pipe", {
#     expect_s3_class(pisaUSA_compare, "bestLPA")
# })
