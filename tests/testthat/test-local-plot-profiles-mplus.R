if(getOption("test_mplus")){
    iris_df <- iris
    names(iris_df) <- gsub("\\.", "_", names(iris_df))
test_that("plot_profiles", {
    out_mplus <- suppressWarnings(estimate_profiles(iris_df[, 1:4], n_profiles = 3, package = "MplusAutomation"))
    expect_s3_class(plot_profiles(out_mplus), "ggplot")
})

test_that("matrix data are correctly read", {
    df <- as.matrix(iris_df[, 1:4])
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"), NA)
})

test_that("Parsing variable names throws error if variable names are not unique", {
    df <- iris_df[, 1:4]
    names(df)[1:2] <- c("Sepal.Length1", "Sepal.Length2")
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"))
})
}
