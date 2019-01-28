context("test-pipe.R")

test_that("pipe is loaded and works", {
  expect_equal({
      c(1,2,3,4,5) %>%
          mean()}, 3)
})
