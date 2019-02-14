context("test-plot-mixtures.R")

results <- iris %>%
    subset(select = c("Sepal.Length", "Sepal.Width",
                      "Petal.Length", "Petal.Width")) %>%
    estimate_profiles(1:3, package = "MplusAutomation")

density_plot <- plot_density(results, variables = "Petal.Length")

test_that("density plot works",
          skip_on_cran(),
          skip_on_travis(),
          expect_s3_class(density_plot, "ggplot")
)
