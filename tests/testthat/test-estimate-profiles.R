context("test-estimate_profiles.R")

m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))

m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")

test_that("estimate_profiles() yields the same estimates for mclust and Mplus", {

  expect_equal(m1[[1]]$estimates$Estimate, m_mplus_1[[1]]$estimates$Estimate, tolerance = .01)

})

test_that("estimate_profiles() yields the same LogLikelihoods for mclust and Mplus", {


expect_equal(round(sapply(m_mclust, function(x){x$fit["LogLik"]}))[c(1,3,2,4)],
          round(sapply(m_mplus, function(x){x$fit["LogLik"]})),
             tolerance = .01)

})
