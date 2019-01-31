context("test-estimate_profiles.R")

m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))
m_cars_mclust <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2)

test_that("estimate_profiles_mclust handles single-column data",
          expect_equal(m_cars_mclust$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429), tolerance = .05))

if(MplusAutomation::mplusAvailable() == 0){
    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation")
    m_cars_mplus <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2, package = "MplusAutomation")

    test_that("estimate_profiles_mplus handles single-column data",
              expect_equal(m_cars_mplus$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429), tolerance = .05))

    test_that("single column fit indices are the same across mplus and mclust",
              expect_equal(m_cars_mclust$model_2_class_2$fit[-length(m_cars_mclust$model_2_class_2$fit)],
              m_cars_mplus$model_2_class_2$fit[-length(m_cars_mplus$model_2_class_2$fit)], tolerance = .05))


    test_that("estimate_profiles() yields the same estimates for mclust and Mplus", {
        expect_equal(m_mclust[[1]]$estimates$Estimate, m_mplus[[1]]$estimates$Estimate, tolerance = .01)
    })

    test_that("estimate_profiles() yields the same LogLikelihoods for mclust and Mplus", {
        expect_equal(round(sapply(m_mclust, function(x){x$fit["LogLik"]})),
                     round(sapply(m_mplus, function(x){x$fit["LogLik"]})),
                     tolerance = .01)
    })


}
