context("as_tibble.tt_lst")

test_that("as_tibble.tt_lst outputs the expected dataframe", {
  tt_lst <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)

  result <- as_tibble(tt_lst)
  expect_is(result, c("tt_df"))

  metrics <-
    c("N.Hab", "Gr.Hab", "Ls.Hab", "Eq.Hab", "Rep.Agg.Neut", "Obs.Quantile")
  expect_true(all(c("habitat", "sp", metrics) %in% names(result)))

  expect_true(all(vapply(result[metrics], is.numeric, logical(1))))
})



context("as.data.frame.tt_lst")

test_that("as.data.frame.tt_lst outputs the expected dataframe", {
  tt_lst <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)

  expect_equal(
    class(as.data.frame(tt_lst)),
    "data.frame"
  )
})

test_that("as.data.frame.tt_lst takes arguments via `...`", {
  tt_lst <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)
  expect_is(as.data.frame(tt_lst)[["sp"]], "factor")

  expect_is(
    as.data.frame(tt_lst, stringsAsFactors = FALSE)[["sp"]],
    "character"
  )
})



context("as_tibble.demography_ctfs")

census1 <- fgeo.x::tree5
census2 <- fgeo.x::tree6

test_that("With no split, or `split1`, outputs consistent dataframe", {
  nms <- c("N2", "R", "rate", "lower", "upper", "time", "date1", "date2")
  census1 <- fgeo.x::tree5
  census2 <- fgeo.x::tree6

  .x <- recruitment_ctfs(census1, census2)
  expect_is(as_tibble(.x), "data.frame")
  expect_named(as_tibble(.x), nms)

  by <- census1$sp
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(as_tibble(.x), "data.frame")
  expect_named(as_tibble(.x), c("groups", nms))
  # Same
  by <- interaction(census1$sp)
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(as_tibble(.x), "data.frame")
  expect_named(as_tibble(.x), c("groups", nms))

  by <- interaction(census1$sp, census1$quadrat)
  .x <- recruitment_ctfs(census1, census2, split1 = by)
  expect_is(as_tibble(.x), "data.frame")
  expect_named(as_tibble(.x), c("groups", nms))
})

test_that("as_tibble.demography_ctfs outputs different than unclassed result", {
  census1 <- fgeo.x::tree5
  census2 <- fgeo.x::tree6
  result <- recruitment_ctfs(census1, census2, split1 = census1$sp)

  expect_false(
    isTRUE(all.equal(
      as_tibble(result),
      as_tibble(unclass(result))
    ))
  )
})

test_that("as_tibble.demography_ctfs can't handle split2", {
  census1 <- fgeo.x::tree5
  census2 <- fgeo.x::tree6

  expect_warning(
    result <- recruitment_ctfs(
      census1,
      census2,
      split1 = census1$sp,
      split2 = census1$quadrat
    ),
    "`split2` is deprecated"
  )

  expect_error(as_tibble(result), "Can't deal with data created with `split2`")
})

test_that("as_tibble.demography_ctfs is equal to unclassed w/ ungrouped data", {
  result <- recruitment_ctfs(fgeo.x::tree5, fgeo.x::tree6)

  expect_equal(
    as_tibble(result),
    as_tibble(unclass(result))
  )
})

test_that("as_tibble.demography_ctfs and as.data.frame.* output equal", {
  result <- recruitment_ctfs(fgeo.x::tree5, fgeo.x::tree6)

  expect_equal(
    as_tibble(result),
    as.data.frame(result)
  )
})



context("as.data.frame.tt_lst")

test_that("as.data.frame.demography_ctfs takes arguments via `...`", {
  result <- recruitment_ctfs(
    fgeo.x::tree5, fgeo.x::tree6,
    split1 = fgeo.x::tree5$sp
  )

  expect_is(
    as.data.frame(result)[["groups"]],
    "factor"
  )
  expect_is(
    as.data.frame(result, stringsAsFactors = FALSE)[["groups"]],
    "character"
  )
})

# test_that("as.data.frame.tt_lst takes arguments via `...`", {
#   tt_lst <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)
#   expect_is(as.data.frame(tt_lst)[["sp"]], "factor")
#
#   expect_is(
#     as.data.frame(tt_lst, stringsAsFactors = FALSE)[["sp"]],
#     "character"
#   )
# })
