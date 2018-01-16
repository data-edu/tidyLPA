context("test-plot_profiles_lpa.R")

test_that("plot_profiles_lpa_works", {
    x <- create_profiles_lpa(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, model = 1, n_profiles = 3)
    expect_s3_class(plot_profiles_lpa(x), "ggplot")
})

