context("summary.tt_df")

expect_printed_output <- function(object, update = FALSE) {
  file <- rlang::quo_text(rlang::enquo(object))

  testthat::expect_known_output(
    object, paste0("ref-", file),
    print = TRUE, update = update
  )

  invisible(object)
}



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

test_that("summary.tt_lst returns the expected output", {
  explain_tt_lst <- summary(tt_result)
  expect_printed_output(explain_tt_lst)
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

  expect_equal(summary(tt_result), tt_lst)
})



context("summary-regression")

# FIXME: Added for extra safety during dangerous moves. Can remove when done
test_that("prints output equal to reference", {
  skip("Run only during risky refactoring")

  explain_tt_test <- as.data.frame(
    summary(tt_test(fgeo.data::luquillo_tree5_random, fgeo.x::habitat))
  )

  expect_printed_output(explain_tt_test)
})
