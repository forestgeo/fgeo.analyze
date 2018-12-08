context("example_byyr")

test_that("returns known output", {
  expect_known_output(
    example_byyr, "ref-example_byyr", print = TRUE, update = TRUE
  )
})
