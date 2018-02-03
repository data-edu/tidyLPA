context("test-create_profiles_lpa-r.R")

test_that("create_profiles_lpa() works", {
  x <- estimate_profiles_lpa(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3)
  y <- dplyr::group_by(x, profile)
  z <- dplyr::summarize_all(y, mean)

  expect_equal(dplyr::pull(z, Sepal.Length)[1], 5.01, tolerance = .01)
  expect_equal(dplyr::pull(z, Sepal.Length)[2], 5.92, tolerance = .01)
  expect_equal(dplyr::pull(z, Sepal.Length)[3], 6.68, tolerance = .01)

  expect_equal(dplyr::pull(z, Sepal.Width)[1], 3.43, tolerance = .01)
  expect_equal(dplyr::pull(z, Sepal.Width)[2], 2.75, tolerance = .01)
  expect_equal(dplyr::pull(z, Sepal.Width)[3], 3.02, tolerance = .01)

  expect_equal(dplyr::pull(z, Petal.Length)[1], 1.46, tolerance = .01)
  expect_equal(dplyr::pull(z, Petal.Length)[2], 4.32, tolerance = .01)
  expect_equal(dplyr::pull(z, Petal.Length)[3], 5.62, tolerance = .01)

  expect_equal(dplyr::pull(z, Petal.Width)[1], 0.24, tolerance = .01)
  expect_equal(dplyr::pull(z, Petal.Width)[2], 1.35, tolerance = .01)
  expect_equal(dplyr::pull(z, Petal.Width)[3], 2.07, tolerance = .01)
})
