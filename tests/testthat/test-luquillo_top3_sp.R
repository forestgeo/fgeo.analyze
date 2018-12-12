context("luquillo_top3_sp")

find_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  unname(sort(dinfo[["results"]][, "Item"]))
}

test_that("expected datasets", {
  expect_true(any(grepl("luquillo_top3_sp", find_datasets("fgeo.analyze"))))
  expect_is(luquillo_top3_sp, "tbl")
})
