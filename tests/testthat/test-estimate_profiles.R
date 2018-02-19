context("test-estimate_profiles.R")

test_that("estimate_profiles() works in terms of MPlus benchmarks", {
  x <- estimate_profiles(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3)
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

test_that("different models (1-4) for estimate_profiles() works", {
  m1 <- estimate_profiles(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3)
  m2 <- estimate_profiles(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 2, n_profiles = 3)
  m3 <- estimate_profiles(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 3, n_profiles = 3)
  m4 <- estimate_profiles(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 6, n_profiles = 3)

  expect_equal(attributes(m1)$mclust_output$loglik, -361.429, tolerance = .001)
  expect_equal(attributes(m2)$mclust_output$loglik, -256.355, tolerance = .001)
  expect_equal(attributes(m3)$mclust_output$loglik, -307.181, tolerance = .001)
  expect_equal(attributes(m4)$mclust_output$loglik, -180.186, tolerance = .001)
})
