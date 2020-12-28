if(getOption("test_mplus")){
    oldwd <- getwd()
    testdir <- file.path(tempdir(), "estimateprofiles")
    dir.create(testdir)
    setwd(testdir)
    on.exit({
        setwd(oldwd)
        unlink(testdir, recursive = TRUE)
    })

    tmp <- capture_output({
    m_cars_mplus <- estimate_profiles(mtcars[, "mpg"],
                                      n_profiles = 2,
                                      models = 2,
                                      package = "MplusAutomation")
    })
    m_cars_mclust <- estimate_profiles(mtcars[, "mpg"], n_profiles = 2, models = 2)
    test_that("estimate_profiles_mplus handles single-column data", {
        expect_equivalent(m_cars_mplus$model_2_class_2$estimates$Estimate, c(18.481, 18.337, 31.759, 2.429), tolerance = .001)
    })

    test_that("single column fit indices are the same across mplus and mclust", {
        expect_equivalent(m_cars_mclust$model_2_class_2$fit[!names(m_cars_mclust$model_2_class_2$fit) %in% c("BLRT_p")],
                     m_cars_mplus$model_2_class_2$fit[!names(m_cars_mplus$model_2_class_2$fit) %in% c("BLRT_p")], tolerance = .0001)
    })
    iris_df <- iris
    names(iris_df) <- gsub("\\.", "_", names(iris_df))
    tmp <- capture_output({
        m_mplus <- suppressWarnings(estimate_profiles(iris_df[, 1:4], n_profiles = 3,  models = c(1:3,6), package = "MplusAutomation"))
    })
    m_mclust <- estimate_profiles(iris_df[, 1:4], n_profiles = 3,  models = c(1:3,6))

    test_that("estimate_profiles() yields the same estimates for mclust and Mplus", {
        expect_equivalent(m_mclust[[1]]$estimates$Estimate, m_mplus[[1]]$estimates$Estimate, tolerance = .001)
    })

    test_that("estimate_profiles() yields the same LogLikelihoods for mclust and Mplus", {
        expect_equivalent(sapply(m_mclust, function(x){x$fit["LogLik"]}),
                     sapply(m_mplus, function(x){x$fit["LogLik"]}),
                     tolerance = .0001)
    })

    test_that("LogLik values are as expected for mplus", {
        LLs <- c(
            m_mplus$model_1_class_3$model$summaries$LL,
            m_mplus$model_2_class_3$model$summaries$LL,
            m_mplus$model_3_class_3$model$summaries$LL,
            m_mplus$model_6_class_3$model$summaries$LL
        )
        Expected <- c(-361.4295,
                      -307.1808,
                      -256.3547,
                      -180.1858)
        expect_equivalent(LLs, Expected, tolerance = .001)
    })
}
