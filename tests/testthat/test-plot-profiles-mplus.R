context("test-plot_profiles_mplus.R")

if(MplusAutomation::mplusAvailable() == 0){
    test_that("plot_profiles", {
        out_mplus <- suppressWarnings(estimate_profiles(iris[, 1:4], n_profiles = 3, package = "MplusAutomation"))
        expect_s3_class(plot_profiles(out_mplus), "ggplot")
    })

}
