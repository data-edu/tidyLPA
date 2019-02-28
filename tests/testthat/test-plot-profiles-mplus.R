context("test-plot_profiles_mplus.R")

test_that("plot_profiles", {
    skip_on_cran()
    skip_on_travis()
    out_mplus <- suppressWarnings(estimate_profiles(iris[, 1:4], n_profiles = 3, package = "MplusAutomation"))
    expect_s3_class(plot_profiles(out_mplus), "ggplot")
})

test_that("matrix data are correctly read", {
    skip_on_cran()
    skip_on_travis()
    df <- as.matrix(iris[, 1:4])
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"), NA)
})

test_that("Parsing variable names throws error if variable names are not unique", {
    skip_on_cran()
    skip_on_travis()
    df <- iris[, 1:4]
    names(df)[1:2] <- c("Sepal.Length1", "Sepal.Length2")
    expect_error(estimate_profiles(df, n_profiles = 3, package = "MplusAutomation"))
})

test_that("plot profiles works with imputed and scaled data using Mplus", {
    skip_on_cran()
    skip_on_travis()
    pisa_profiles_plot_mplus <- pisaUSA15[1:100, ] %>%
        select(broad_interest, enjoyment, self_efficacy) %>%
        single_imputation() %>%
        scale() %>%
        estimate_profiles(3, package = "MplusAutomation") %>%
        plot_profiles()
    expect_s3_class(estimate_profiles(pisa_profiles_plot, "ggplot"))
})
