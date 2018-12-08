context("utils")

test_that("warns if all dates are missing", {
  c1 <- fgeo.x::tree5
  c2 <- fgeo.x::tree6
  c1$date <- NA
  c2$date <- NA
  expect_warning(time_diff(c1, c2), "all missing")
})
