if(getOption("test_mplus")){
test_that("plot_profiles", {
    out_mplus <- suppressWarnings(estimate_profiles(iris[, 1:4], n_profiles = 3, package = "MplusAutomation"))
    expect_s3_class(plot_profiles(out_mplus), "ggplot")
})

test_that("matrix data are correctly read", {
    df <- as.matrix(iris[, 1:4])
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"), NA)
})

test_that("Parsing variable names no longer throws error if variable names are not unique", {
    df <- iris[, 1:4]
    names(df)[1:2] <- c("Sepal.Length1", "Sepal.Length2")
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"), NA)
})
}
