context("insensitive")

test_that("[, [[, and $ extract elements of named objects ignoring case", {
  x <- c("abc" = 1, "def" = 2)
  y <- insensitive(x)
  expect_named(y["ABC"], "abc")
  expect_equal(y[["ABC"]], 1)
  expect_equal(y$ABC, 1)

  vft <- data.frame(TreeID = 1)
  ivft <- insensitive(vft)
  expect_named(ivft["TreeID"], "treeid")
  expect_equal(ivft[["TreeID"]], 1)
  expect_equal(ivft$TreeID, 1)
})

test_that("equals to setting names to lower case", {
  vft <- data.frame(TreeID = 1)
  ivft <- insensitive(vft)
  expect_equal(
    unclass(ivft),
    unclass(setNames(vft, tolower(names(vft))))
  )
})

test_that("fails with informative errors", {
  expect_warning(insensitive(1), "should be named")
})
