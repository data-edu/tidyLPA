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

    m_cars_mplus_benchmark_stats <- tibble(stat = names(m_cars_mplus$model_1_class_2$fit),
                                           val = m_cars_mplus$model_1_class_2$fit)

    m_cars_clust_benchmark_stats <- tibble(stat = names(m_cars_mclust$model_1_class_2$fit),
                                           val = m_cars_mclust$model_1_class_2$fit)

    m_cars_mplus_benchmark_stats <- m_cars_mplus_benchmark_stats[m_cars_mplus_benchmark_stats$stat %in% tab_benchmark$stat, ]
    m_cars_mclust_benchmark_stats <- m_cars_mplus_benchmark_stats[m_cars_mplus_benchmark_stats$stat %in% tab_benchmark$stat, ]

    expect_equal(m_cars_mplus_benchmark_stats$val, tab_benchmark$val, tolerance = .05)
    expect_equal(m_cars_mclust_benchmark_stats$val, tab_benchmark$val, tolerance = .05)
})
