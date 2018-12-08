context("tt_test.R")

# Ensure consistent values accross runs
set.seed(123)
library(dplyr)

# Small dataset from Luquillo
cns_luq <- luquillo_top3_sp
sp_top3_luq <- unique(cns_luq$sp)
hab_luq <- fgeo.x::habitat
sp_top1_luq <- first(sp_top3_luq)



test_that("outputs the expected list", {
  out <- expect_message(tt_test(cns_luq, sp_top1_luq, hab_luq), "Using")
  expect_equal(class(out), c("tt_lst", "list"))
  expect_equal(dim(out[[1]]), c(1, 24))
  expect_equal(sp_top1_luq, rownames(out[[1]]))
})

test_that("prints as an unclassed list (i.e. doesn't show attr ...)", {
  output <- capture_output(print(tt_test(cns_luq, sp_top1_luq, hab_luq)))
  expect_false(grepl("tt_lst", output))
})



pdim_luq <- c(320, 500)
gsize_luq <- 20

abnd <- abund_index(cns_luq, pdim_luq, gsize_luq)
out_tt <- torusonesp.all(sp_top1_luq, hab_luq, abnd, pdim_luq, gsize_luq)

test_that("outputs expected values", {
  out_lst <- tt_test(cns_luq, sp_top1_luq, hab_luq)
  expect_equal(unclass(out_lst[[1]]), unclass(out_tt))
})

test_that("species may be factor or character", {
  expect_true(
    identical(
      tt_test(cns_luq, as.factor(sp_top1_luq), hab_luq),
      tt_test(cns_luq, sp_top1_luq, hab_luq)
    )
  )
})

test_that("fails with informative message", {
  expect_error(
    tt_test(cns_luq, 1, hab_luq),
    "`sp` must be of class character or factor"
  )
  expect_error(tt_test(cns_luq, "a", hab_luq), "All `sp` must be present")
  expect_error(
    tt_test(cns_luq, c("SLOBER", "odd"), hab_luq),
    "odd"
  )

  expect_error(
    tt_test(census = 1, c("SLOBER", "PREMON"), hab_luq),
    "is not TRUE"
  )

  expect_error(tt_test(cns_luq, c("SLOBER", "PREMON"), 1), "is not TRUE")
  expect_error(tt_test(cns_luq, c("SLOBER"), hab_luq, 1), "is not TRUE")
  expect_error(
    tt_test(cns_luq, c("SLOBER"), hab_luq, pdim_luq, "a"),
    "is not TRUE"
  )
  expect_warning(
    tt_test(cns_luq, c("SLOBER"), hab_luq, pdim_luq, 12),
    "Uncommon `gridsize`"
  )
})

test_that("warns if census it not a tree table (#33)", {
  # Run only local. Buildignoring stem.rds to save space in the source package
  skip_if(!file.exists(test_path("stem.rds")))
  stem <- readRDS(test_path("stem.rds"))

  msg <- "Is `census` a tree table"
  expect_warning(
    suppressMessages(tt_test(
      stem,
      sp_top1_luq, hab_luq
    )),
    msg
  )

  expect_silent(suppressMessages(
    tt_test(
      tree_table_throws_no_warning <- cns_luq,
      sp_top1_luq, hab_luq
    )
  ))
})

test_that("warns if habitat data isn't of class fgeo_habitat", {
  hab_luq2 <- hab_luq
  class(hab_luq2) <- setdiff(class(hab_luq2), "fgeo_habitat")
  msg <- "isn't of class 'fgeo_habitat'"
  expect_warning(tt_test(cns_luq, sp_top1_luq, hab_luq2), msg)
})

test_that("with habitat data with names gx,gy|x,y output is identical", {
  hab_luq3 <- hab_luq
  class(hab_luq3) <- setdiff(class(hab_luq3), "fgeo_habitat")
  hab_luq3 <- setNames(hab_luq3, c("x", "y", "habitats"))

  out_gxgy <- tt_test(cns_luq, sp_top1_luq, hab_luq)
  out_xy <- suppressWarnings(tt_test(cns_luq, sp_top1_luq, hab_luq3))
  identical(out_xy, out_gxgy)
})



# Issue 30, Sabrina Russo ----------------------------------------------------

# Original explanation of the error:
# The observed quantile for habitat 2 (Obs.Quantile.2) is 0.0094, so that's <
# 0.025, so it should get a "-1" {minus one} instead of "0" in the
# "Rep.Agg.Neut.2" column.

can_access_private_data <- dir.exists(test_path("private"))
if (can_access_private_data) {
  hab.index20 <- get(load(test_path("private/russo_30/hab.index20.Rdata")))
  allabund20 <- get(load(test_path("private/russo_30/allabund20.Rdata")))

  out <- torusonesp.all(
    species = "KNEMLA",
    hab.index20 = hab.index20,
    allabund20 = allabund20,
    plotdim = c(1000, 500),
    gridsize = 20
  )

  actual <- out[colnames(out) %in% "Rep.Agg.Neut.2"]
  test_that("torusonesp.all() w/ spp KNEMLA returns as expected (#30, Russo)", {
    # Only runs for those with access to private/ data
    skip_if(!can_access_private_data)

    expect_equal(actual, -1)
    expect_error(expect_equal(actual, 0), "not equal to 0")
  })
}



# @RutujaCT #44 ------------------------------------------------------------

test_that("tt_test() with rutuja's data warns that `habitat` is problematic", {
  can_access_private_data <- dir.exists(test_path("private"))
  # Only runs for those with access to private/ data
  skip_if_not(can_access_private_data)

  rutu <- get(load(test_path("private/RutujaCT_44/tt_test_data.Rdata")))
  msg <- "`habitat` isn't of class 'fgeo_habitat'"
  expect_error(
    expect_warning(tt_test(rutu$pick, rutu$few_sp, rutu$habitat2), msg)
  )
})
