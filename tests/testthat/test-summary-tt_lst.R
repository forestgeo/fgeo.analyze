context("summary.tt_test()")

result <- summary(tt_test(fgeo.x::tree6_3species, fgeo.x::habitat))

test_that("returns the expected output", {
  result %>%
    expect_known_output("ref-summary-tt_lst", print = TRUE)
})

test_that("outputs a tibble", {
  result %>%
    expect_is("tbl")
})

test_that("result has all columns are of base type 'character'", {
  columns_are_of_type_character <- all(vapply(result, is.character, logical(1)))
  columns_are_of_type_character %>%
    expect_true()
})

test_that("outputs a `species` column with the species names", {
  result[["species"]] %>%
    expect_equal(c("CASARB", "PREMON", "SLOBER"))
})

test_that("outputs has the expected (lowercase) names", {
  result %>%
    expect_named(
      c("species", "habitat_1", "habitat_2", "habitat_3", "habitat_4")
  )
})
