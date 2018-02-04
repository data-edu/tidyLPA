# context("test-create_profiles_mplus-r.R")
#
# test_that("create_profiles_mplus() works", {
#     x <- create_profiles_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3)[[1]]
#     y <- as.data.frame(suppressWarnings(x$parameters$unstandardized[x$parameters$unstandardized$paramHeader == "Means", ]))
#
#     expect_equal(y[1, 3], 5.01, tolerance = .01)
#     expect_equal(y[5, 3], 5.92, tolerance = .01)
#     expect_equal(y[9, 3], 6.68, tolerance = .01)
#
#     expect_equal(y[2, 3], 3.43, tolerance = .01)
#     expect_equal(y[6, 3], 2.75, tolerance = .01)
#     expect_equal(y[10, 3], 3.02, tolerance = .01)
#
#     expect_equal(y[3, 3], 1.46, tolerance = .01)
#     expect_equal(y[7, 3], 4.32, tolerance = .01)
#     expect_equal(y[11, 3], 5.62, tolerance = .01)
#
#     expect_equal(y[4, 3], 0.24, tolerance = .01)
#     expect_equal(y[8, 3], 1.35, tolerance = .01)
#     expect_equal(y[12, 3], 2.07, tolerance = .01)
# })
