context("test-estimate_profiles-mclust.R")

m_mclust <- estimate_profiles(iris[, 1:4], n_profiles = 3,  models = c(1:3,6))
m_cars_mclust <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2)

test_that("estimate_profiles_mclust handles single-column data",
          expect_equal(m_cars_mclust$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429),
                       tolerance = .05))

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

m_pisa_mclust_3_profiles <- estimate_profiles(pisaUSA15[1:100, ], n_profiles = 3)

test_that("estimate_profiles works with build-in PISA data and mclust",
          expect_s3_class(m_pisa_mclust_3_profiles, "tidyLPA")
)
