context("check_crucial_names.R")

test_that("passes and fails as expected", {
  expect_silent(check_crucial_names(c(x = 1), "x"))
  expect_error(check_crucial_names(c(x = 1), "y"))
  expect_silent(check_crucial_names(data.frame(x = 1), "x"))
  expect_error(check_crucial_names(data.frame(x = 1), "y"))
})
