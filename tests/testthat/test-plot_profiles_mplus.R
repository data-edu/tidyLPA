context("test-plot_profiles_mplus.R")

test_that("plot_profiles_mplus_works", {
    skip_on_cran()
    skip_on_travis()
    x <- estimate_profiles_mplus(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3, return_save_data = T)
    expect_s3_class(plot_profiles_mplus(x), "ggplot")
})
