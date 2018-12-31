context("abundanceperquad2")

test_that("outputs equal to reference from ctfs", {
  skip_if_not_installed("ctfs")

  ref <- ctfs::abundanceperquad(
    fgeo.x::tree5,
    mindbh = 10,
    plotdim = c(300, 500),
    gridsize = 20,
    type = "abund"
  )

  act <- abundanceperquad2(
    fgeo.x::tree5,
    mindbh = 10,
    plotdim = c(300, 500),
    gridsize = 20,
    type = "abund"
  )

  expect_equal(act$abund, ref$abund)
})

test_that("stops if type is not abund", {
  expect_error(abundance2(fgeo.x::tree5, type = "bad"), "must be.*abund")
})

test_that("works with simple input", {
  expect_error(abundance2(fgeo.x::tree5), NA)
})
