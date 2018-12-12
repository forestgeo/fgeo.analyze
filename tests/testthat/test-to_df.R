context("to_df.tt_lst")

describe("to_df.tt_lst", {
  skip_if_not_installed("fgeo.analyze")

  cns <- fgeo.analyze::luquillo_top3_sp
  spp <- unique(cns$sp)[1]
  hab_luq <- fgeo.x::habitat
  tt_lst <- fgeo.analyze::tt_test(cns, spp, hab_luq)

  it("outputs the expected dataframe", {
    expect_equal(class(tt_lst), c("tt_lst", "list"))

    out <- expect_silent(to_df(tt_lst))
    expect_is(out, c("tt_df"))
    metrics <-
      c("N.Hab", "Gr.Hab", "Ls.Hab", "Eq.Hab", "Rep.Agg.Neut", "Obs.Quantile")
    expect_true(all(c("habitat", "sp", metrics) %in% names(out)))

    expect_true(all(vapply(out[metrics], is.numeric, logical(1))))
  })
})



context("to_df.demography_ctfs")

census1 <- fgeo.x::tree5
census2 <- fgeo.x::tree6

test_that("with split2 errs with informative message", {
  skip("Quarentine")
  expect_warning(
    out <- recruitment_ctfs(
      census1, census2, split1 = census1$sp,
      split2 = census1$quadrat
    ), "split2.*deprecated"
  )
  expect_error(to_df(out), "split2.*deprecated")
})

test_that("With no split, or `split1`, outputs consistent dataframe", {
  nms <- c("N2", "R", "rate", "lower", "upper", "time", "date1", "date2")

  .x <- recruitment_ctfs(census1, census2)
  expect_error(to_df(unclass(.x)), "Can't deal with data")

  .x <- recruitment_ctfs(census1, census2)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), nms)

  by <- census1$sp
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))
  # Same
  by <- interaction(census1$sp)
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))

  by <- interaction(census1$sp, census1$quadrat)
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))
})

