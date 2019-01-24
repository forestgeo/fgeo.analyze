context("fgeo_habitat")

test_that("fgeo_habitat outputs identical with elevation list or dataframe", {
  census <- dplyr::filter(fgeo.x::tree6_3species, status == "A", dbh >= 10)

  elev_ls <- fgeo.x::elevation
  habitat_ls <- fgeo_habitat(
    elev_ls,
    gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
  )

  elev_df <- fgeo.x::elevation$col
  habitat_df <- fgeo_habitat(
    elev_df,
    gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
  )

  expect_identical(habitat_ls, habitat_df)
})

test_that("fgeo_habitat outputs object that throws no warning with tt_test()", {
  census <- dplyr::filter(fgeo.x::tree6_3species, status == "A", dbh >= 10)
  habitat <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
  expect_message(tt_test(census, habitat = habitat))
})

test_that("fgeo_habitat errs with informative messages", {
  elev_ls <- fgeo.x::elevation
  gridsize <- 20
  plotdim <- c(320, 500)
  habitat <- fgeo_habitat(elev_ls, gridsize = 20, n = 4)

  expect_error(fgeo_habitat(1), "Can't deal with.*numeric")
  expect_error(fgeo_habitat(elev_ls), "gridsize.*is missing")
  expect_error(fgeo_habitat(elev_ls, 20), "n.*is missing")
  elev_ls_missing_xdim <- elev_ls
  elev_ls_missing_xdim$xdim <- NULL
  expect_error(fgeo_habitat(elev_ls_missing_xdim), "gridsize.*is missing")
  expect_error(
    fgeo_habitat(elev_ls$col, gridsize = 20), "xdim.*ydim.*can't be `NULL`"
  )
})

test_that("fgeo_habitat plots with plot.fgeo_habitat()", {
  skip_if_not_installed("fgeo.plot")
  library(fgeo.plot)

  habitat <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
  expect_is(autoplot(habitat), "ggplot")
})

test_that("fgeo_habitat results in gx and gy that are multiple of gridsize", {
  gridsize <- 20
  habitat <- fgeo_habitat(fgeo.x::elevation, gridsize = gridsize, n = 4)
  expect_true(all(habitat$gx %% gridsize == 0))
})

test_that("fgeo_habitat is sensitive to `edgecorrect`", {
  habitat <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
  out <- fgeo_habitat(
    fgeo.x::elevation,
    gridsize = 20, n = 4, edgecorrect = FALSE
  )
  expect_false(identical(out, habitat))
})

test_that("fgeo_habitat w/ `pasoh` outputs rows equal to num of quadrats", {
  skip_if_not_installed("pasoh")

  gridsize <- 20
  plotdim <- c(320, 500)
  habitat <- fgeo_habitat(fgeo.x::elevation, gridsize = gridsize, n = 4)

  rows <- plotdim[[1]] / gridsize
  cols <- plotdim[[2]] / gridsize
  expect_equal(nrow(habitat), rows * cols)

  # Reference: This habitat dataset was created by the authors of tt_test()
  habitat_pasoh <- pasoh::pasoh_hab_index20
  pd <- c(1000, 500)
  gs <- 20
  rw <- pd[[1]] / gs
  cl <- pd[[2]] / gs
  expect_equal(nrow(habitat_pasoh), rw * cl)
})
