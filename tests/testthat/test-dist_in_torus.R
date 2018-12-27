context("dist_in_torus")

test_that("outputs is a matrix of doubles", {
  x <- matrix(0:3, nrow = 2)
  d1 <- dist_in_torus(x)
  expect_type(d1, "double")
  expect_true("matrix" %in% class(d1))

  d2 <- dist_in_torus(x, lower = c(3, 13), upper = c(5, 15))
  expect_type(d2, "double")
  expect_true("matrix" %in% class(d2))
})

test_that("fails with wrong input", {
  chr <- matrix(letters[1:4], nrow = 2)
  expect_error(
    dist_in_torus(chr), "`x` must be numeric"
  )

  x <- matrix(1:9, nrow = 3)
  expect_error(dist_in_torus(x, lower = c(0, 0), upper = c(1, 1, 1)))
  expect_error(dist_in_torus(x, lower = c(0, 0, 0), upper = c(1, 1)))
})

test_that("warns if input is not a matrix", {
  x <- matrix(1:4, nrow = 2)
  expect_warning(
    dist_in_torus(as.data.frame(x)), "Coercing `x` to matrix."
  )

  x <- 1:4
  expect_warning(
    dist_in_torus(x)
  )
})

test_that("behaves in particular ways with extreeme conditions", {
  # This is mainly to document the behaviour. Not to say it is OK. I don't know.
  # Passes
  expect_silent(dist_in_torus(matrix(c(NaN, NaN))))
  expect_silent(dist_in_torus(matrix(c(Inf, Inf))))
  expect_silent(dist_in_torus(matrix(c(1, 1))))
  expect_silent(dist_in_torus(matrix(c(-1, -1))))
  expect_silent(dist_in_torus(matrix(c(1, 1))))
  # Warns
  expect_warning(dist_in_torus(data.frame(a = c(1, 1))))
  # Fails
  expect_error(dist_in_torus(matrix(c(NA, NA))))
  expect_error(dist_in_torus(matrix(1)))
  expect_error(dist_in_torus(matrix(c(NULL, NULL))))
})
