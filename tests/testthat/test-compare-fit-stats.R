context("test-compare-fit-stats.R")

test_that("fit stats are the same for both mplus and mclust", {
    skip_on_cran()
    skip_on_travis()

    tab_mclust <- tibble::tribble(
        ~stat,         ~val,
        "Model",            1,
        "Classes",            2,
        "LogLik", -271.2943387,
        "AIC",  556.5886774,
        "AWE",  610.4725311,
        "BIC",  566.8488288,
        "CAIC",  573.8488288,
        "CLC",  544.2251264,
        "KIC",  566.5886774,
        "SABIC",  545.0268243,
        "ICL", -569.7289846,
        "Entropy",  0.818224466,
        "prob_min",   0.95731999,
        "prob_max",  0.960443609,
        "n_min",      0.46875,
        "n_max",      0.53125,
        "BLRT_val",  22.53901766,
        "BLRT_p",   0.00990099
    )

    # for benchmarking
    # createMixtures(classes = 2, filename_stem = "mtcars-mpg-hp", rdata = mtcars[, c("mpg", "hp")])
    # runModels(target = "mtcars-mpg-hp_2_class.inp")
    # o <- readModels("mtcars-mpg-hp_2_class.out")

    tab_benchmark <- tibble::tribble( # these are from benchmarking code above
        ~stat,         ~val,
        "Model",            1,
        "Classes",            2,
        "LogLik",      -271.292,
        "AIC",      556.584,
        "BIC",  566.844,
        "Entropy",        0.819,
        "BLRT_val",         22.544
    )

    m_cars_mplus <- estimate_profiles(mtcars[, c("mpg", "hp")],
                                      n_profiles = 2,
                                      models = 1,
                                      package = "MplusAutomation")

    m_cars_mclust <- estimate_profiles(mtcars[, c("mpg", "hp")],
                                       n_profiles = 2,
                                       models = 1)

    expect_equal(as.vector(m_cars_mplus$model_1_class_2$fit), tab_mclust$val, tolerance = .05)
    expect_equal(as.vector(m_cars_mclust$model_1_class_2$fit), tab_mclust$val, tolerance = .05)
    expect_equal(as.vector(m_cars_mplus$model_1_class_2$fit), as.vector(m_cars_mplus$model_1_class_2$fit), tolerance = .05)

    m_cars_mplus$model_1_class_2$fit

    kept_cases <- complete.cases(pisaUSA15[, c("broad_interest", "enjoyment", "self_efficacy")])
    m_pisa_mplus <- estimate_profiles(pisaUSA15[kept_cases, c("broad_interest", "enjoyment", "self_efficacy")],
                                      n_profiles = 3,
                                      models = 1,
                                      package = "MplusAutomation")
    m_pisa_mclust <- estimate_profiles(pisaUSA15[kept_cases, c("broad_interest", "enjoyment", "self_efficacy")],
                                       n_profiles = 3,
                                       models = 1)

    expect_equal(as.vector(m_pisa_mplus$model_1_class_2$fit), as.vector(m_pisa_mplus$model_1_class_2$fit), tolerance = .05)

    m_cars_mplus_benchmark_stats <- tibble(stat = names(m_cars_mplus$model_1_class_2$fit),
                                           val = m_cars_mplus$model_1_class_2$fit)

    m_cars_clust_benchmark_stats <- tibble(stat = names(m_cars_mclust$model_1_class_2$fit),
                                           val = m_cars_mclust$model_1_class_2$fit)

    m_cars_mplus_benchmark_stats <- m_cars_mplus_benchmark_stats[m_cars_mplus_benchmark_stats$stat %in% tab_benchmark$stat, ]
    m_cars_mclust_benchmark_stats <- m_cars_mplus_benchmark_stats[m_cars_mplus_benchmark_stats$stat %in% tab_benchmark$stat, ]

    expect_equal(m_cars_mplus_benchmark_stats$val, tab_benchmark$val, tolerance = .05)
    expect_equal(m_cars_mclust_benchmark_stats$val, tab_benchmark$val, tolerance = .05)
})

test_that("fit stats are equal to those from models estimated externally in Mplus", {
    skip_on_cran()
    skip_on_travis()

    # Model 1
    m_cars_mplus_model1 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                             n_profiles = 2,
                                             models = 1,
                                             package = "MplusAutomation")

    m_cars_mclust_model1 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                              n_profiles = 2,
                                              models = 1)

    # Model 2
    m_cars_mplus_model2 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                             n_profiles = 2,
                                             models = 2,
                                             package = "MplusAutomation")

    m_cars_mclust_model2 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                              n_profiles = 2,
                                              models = 2)

    # Model 3
    m_cars_mplus_model3 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                             n_profiles = 2,
                                             models = 3,
                                             package = "MplusAutomation")

    m_cars_mclust_model3 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                              n_profiles = 2,
                                              models = 3)

    # Model 6
    m_cars_mplus_model6 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                             n_profiles = 2,
                                             models = 6,
                                             package = "MplusAutomation")

    m_cars_mclust_model6 <- estimate_profiles(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
                                              n_profiles = 2,
                                              models = 6)

    iris_log_lik <- tribble(
        ~model, ~logLik,
        1, -488.915,
        2, -386.185,
        3, -296.448,
        6, -214.355
    )

    iris_log_lik$mplus <- c(m_cars_mplus_model1$model_1_class_2$fit[3],
                            m_cars_mplus_model2$model_2_class_2$fit[3],
                            m_cars_mplus_model3$model_3_class_2$fit[3],
                            m_cars_mplus_model6$model_6_class_2$fit[3])

    iris_log_lik$mclust <- c(m_cars_mclust_model1$model_1_class_2$model$loglik,
                             m_cars_mclust_model2$model_2_class_2$model$loglik,
                             m_cars_mclust_model3$model_3_class_2$model$loglik,
                             m_cars_mclust_model6$model_6_class_2$model$loglik)

    expect_equal(iris_log_lik$logLik, iris_log_lik$mplus,
                 tolerance = .05)

    expect_equal(iris_log_lik$logLik, iris_log_lik$mclust,
                 tolerance = .001)

})
