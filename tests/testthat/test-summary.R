context("summary.tt_df")

tt_result <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)

test_that("summary.tt_df returns the expected data structure", {
  expect_is(summary(as_tibble(tt_result)), "tbl_df")
  expect_named(summary(as_tibble(tt_result)), c("sp", "habitat", "association"))
})



context("summary.tt_lst")

test_that("summary.tt_df returns the expected data structure", {
  expect_is(summary(tt_result), "tbl_df")
  expect_named(summary(as_tibble(tt_result)), c("sp", "habitat", "association"))
})

test_that("summary.tt_lst returns the expected value", {
  expect_known_value(summary(tt_result), test_path("ref-summary-tt_result"))
})

test_that("summary.tt_lst result has all columns are of base type 'character'", {
  all_character <- all(vapply(summary(tt_result), is.character, logical(1)))
  expect_true(all_character)
})

test_that("summary.tt_lst outputs a `sp` column with the species names", {
  expect_equal(
    unique(summary(tt_result)[["sp"]]),
    c("CASARB", "PREMON", "SLOBER")
  )
})



context("explain")

test_that("summary.tt_lst and summary.tt_df return equal", {
  tt_lst <- summary(as_tibble(tt_result)) %>%
    select(habitat, sp, association) %>%
    arrange(sp, habitat) %>%
    purrr::modify(as.character)

  expect_equivalent(
    summary(tt_result)[names(tt_lst)],
    tt_lst
  )
})
