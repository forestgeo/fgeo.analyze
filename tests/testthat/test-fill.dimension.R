context("fill.dimension")

test_that("fill.dimension() doesn't change with refactoring", {
  skip_if_not_installed("ctfs")

  dataarray <- readRDS(testthat::test_path("dataarray.rds"))
  class1 <- c("CASARB", "PREMON", "SLOBER")
  class2 <- readRDS(testthat::test_path("class2.rds"))
  ctfs <- ctfs::fill.dimension(dataarray, class1, class2, fill = 0)
  hab <- fill.dimension(dataarray, class1, class2, fill = 0)
  expect_identical(ctfs, hab)
})
