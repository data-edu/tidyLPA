results <- estimate_profiles(subset(iris, select = c("Sepal.Length", "Sepal.Width",
                                        "Petal.Length", "Petal.Width")), 1:3)
density_plot <- plot_density(results, variables = "Petal.Length")

test_that("density plot works",
          expect_is(density_plot, "gg")
)
