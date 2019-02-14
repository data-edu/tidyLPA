context("test-estimate_profiles.R")

# Mplus tests

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

    m_mplus <- estimate_profiles(iris[, 1:4], n_profiles = 3,
                                 models = c(1:3,6), package = "MplusAutomation")

    test_that("LogLik values are as expected for model type 1",
              expect_equal(m_mplus$model_1_class_3$model$summaries$LL, -361.4295,
                           tolerance = .001)
    )

    test_that("LogLik values are as expected for model type 2",
              expect_equal(m_mplus$model_2_class_3$model$summaries$LL, -307.1808,
                           tolerance = .001)
    )

    test_that("LogLik values are as expected for model type 3",
              expect_equal(m_mplus$model_3_class_3$model$summaries$LL, -256.3547,
                           tolerance = .001)
    )

    test_that("LogLik values are as expected for model type 6",
              expect_equal(m_mplus$model_6_class_3$model$summaries$LL, -180.1858,
                           tolerance = .001)
    )
}

# Mclust tests

library(dplyr)

m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))

m_cars_mclust <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2)

test_that("estimate_profiles_mclust handles single-column data",
          expect_equal(m_cars_mclust$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429), tolerance = .05))

test_that("LogLik values are as expected for model type 1",
          expect_equal(m_mclust$model_1_class_3$model$loglik, -361.4295,
                       tolerance = .001)
          )

test_that("LogLik values are as expected for model type 2",
          expect_equal(m_mclust$model_2_class_3$model$loglik, -307.1808,
                       tolerance = .001)
)

test_that("LogLik values are as expected for model type 3",
          expect_equal(m_mclust$model_3_class_3$model$loglik, -256.3547,
                       tolerance = .001)
)

test_that("LogLik values are as expected for model type 6",
          expect_equal(m_mclust$model_6_class_3$model$loglik, -180.1858,
                       tolerance = .001)
)

m_iris_mclust_3_profiles <- iris %>%
    select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
    estimate_profiles(3)

test_that("estimate_profiles works with iris data and mclust",
          expect_s3_class(m_iris_mclust_3_profiles, "tidyLPA")
)

m_pisa_mclust_3_profiles <- pisaUSA15[1:100, ] %>%
    select(broad_interest, enjoyment, self_efficacy) %>%
    estimate_profiles(3)

test_that("estimate_profiles works with build-in PISA data and mclust",
          expect_s3_class(m_pisa_mclust_3_profiles, "tidyLPA")
)

m_pisa_mclust_3_profiles_imputation <- pisaUSA15[1:100, ] %>%
    select(broad_interest, enjoyment, self_efficacy) %>%
    single_imputation() %>%
    estimate_profiles(3)

test_that("estimate_profiles works when using single imputation and mclust",
          expect_s3_class(m_pisa_mclust_3_profiles_imputation, "tidyLPA")
)
