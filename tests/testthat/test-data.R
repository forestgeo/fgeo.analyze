context("data")

find_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  unname(sort(dinfo[["results"]][, "Item"]))
}

test_that("expected datasets", {
  actual <- find_datasets("fgeo.analyze")
  expect_true(any(grepl("luquillo_top3_sp", actual)))
})
