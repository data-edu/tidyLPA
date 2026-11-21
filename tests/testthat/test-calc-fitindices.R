test_that("EEE parameter count agrees with mclust", {

    set.seed(123)
    dat <- iris[, 1:4]
    fit <- estimate_profiles(dat, n_profiles = 2, models = 3, package = "mclust")
    expect_equal(as.numeric(fit[[1]]$fit[, "parameters"]), fit[[1]]$model$df)
})


test_that("VVV parameter count agrees with mclust", {

    set.seed(123)

    dat <- iris[, 1:4]

    fit <- estimate_profiles(dat, n_profiles = 2, models = 6, package = "mclust")

    expect_equal(as.numeric(fit[[1]]$fit[, "parameters"]), fit[[1]]$model$df)
})