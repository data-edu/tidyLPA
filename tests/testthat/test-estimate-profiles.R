context("test-estimate_profiles.R")

m1 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 1)
m2 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 2)
m3 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 3)
m6 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 6)

m_mplus_1 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 1, package = "MplusAutomation")
m_mplus_2 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 2, package = "MplusAutomation")
m_mplus_3 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 3, package = "MplusAutomation")
m_mplus_6 <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = 6, package = "MplusAutomation")

test_that("estimate_profiles() yields the same estimates for mclust and Mplus", {

  expect_equal(m1[[1]]$estimates$Estimate, m_mplus_1[[1]]$estimates$Estimate, tolerance = .01)

})

test_that("estimate_profiles() yields the same LogLikelihoods for mclust and Mplus", {


expect_equal(sapply(list(m1, m2, m3, m6), function(x){x[[1]]$fit[["LogLik"]]}),
             sapply(list(m_mplus_1, m_mplus_2, m_mplus_3, m_mplus_6), function(x){x[[1]]$fit[["LogLik"]]}),
             tolerance = .01)

})
