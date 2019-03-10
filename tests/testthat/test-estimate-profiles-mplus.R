context("test-estimate_profiles-mplus.R")

test_that("estimate_profiles_mplus handles single-column data", {
    skip_on_cran()
    skip_on_travis()
    m_cars_mplus <- estimate_profiles(mtcars[, "mpg"],
                                      n_profiles = 2,
                                      models = 2,
                                      package = "MplusAutomation")
    expect_equal(m_cars_mplus$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429), tolerance = .05)
})

test_that("single column fit indices are the same across mplus and mclust", {
    skip_on_cran()
    skip_on_travis()
    m_cars_mplus <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2, package = "MplusAutomation")
    m_cars_mclust <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2)
    expect_equal(m_cars_mclust$model_2_class_2$fit[-length(m_cars_mclust$model_2_class_2$fit)],
                 m_cars_mplus$model_2_class_2$fit[-length(m_cars_mplus$model_2_class_2$fit)], tolerance = .05)
})

test_that("estimate_profiles() yields the same estimates for mclust and Mplus", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))
    expect_equal(m_mclust[[1]]$estimates$Estimate, m_mplus[[1]]$estimates$Estimate, tolerance = .01)
})

test_that("estimate_profiles() yields the same LogLikelihoods for mclust and Mplus", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))
    expect_equal(round(sapply(m_mclust, function(x){x$fit["LogLik"]})),
                 round(sapply(m_mplus, function(x){x$fit["LogLik"]})),
                 tolerance = .01)
})

test_that("LogLik values are as expected for model type 1", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,
                                 models = c(1:3,6), package = "MplusAutomation")
    expect_equal(m_mplus$model_1_class_3$model$summaries$LL, -361.4295,
                 tolerance = .001)
})

test_that("LogLik values are as expected for model type 2", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    expect_equal(m_mplus$model_2_class_3$model$summaries$LL, -307.1808,
                 tolerance = .001)
})

test_that("LogLik values are as expected for model type 3", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    expect_equal(m_mplus$model_3_class_3$model$summaries$LL, -256.3547,
                 tolerance = .001)
})

test_that("LogLik values are as expected for model type 6", {
    skip_on_cran()
    skip_on_travis()
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    expect_equal(m_mplus$model_6_class_3$model$summaries$LL, -180.1858,
                 tolerance = .001)
})
