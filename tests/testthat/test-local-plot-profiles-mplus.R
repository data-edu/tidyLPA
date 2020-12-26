if(getOption("test_mplus")){
    oldwd <- getwd()
    testdir <- file.path(tempdir(), "plotmplus")
    dir.create(testdir)
    setwd(testdir)
    on.exit({
        setwd(oldwd)
        unlink(testdir, recursive = TRUE)
    })

    iris_df <- iris
    names(iris_df) <- gsub("\\.", "_", names(iris_df))
test_that("plot_profiles", {
    tmp <- capture_output({
        out_mplus <- suppressWarnings(estimate_profiles(iris_df[, 1:4], n_profiles = 3, package = "MplusAutomation"))
    })

    expect_s3_class(plot_profiles(out_mplus), "ggplot")
})

test_that("matrix data are correctly read", {
    df <- as.matrix(iris_df[, 1:4])
    tmp <- capture_output({
        expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"), NA)
    })
})

test_that("Parsing variable names throws error if variable names are not unique", {
    df <- iris_df[, 1:4]
    names(df)[1:2] <- c("Sepal.Length1", "Sepal.Length2")
    tmp <- capture_output({
        expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"))
    })
})
}
