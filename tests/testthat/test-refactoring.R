original_find.climits <- function(N, D, alpha = .05, kind = "upper") {
  if (kind == "lower") {
    result <- N * (1 - qbeta(1 - alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == 0] <- 0
  }
  else if (kind == "upper") {
    result <- N * (1 - qbeta(alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == N] <- N[D == N]
  }

  return(result)
}

refactored_find.climits <- function(N, D, alpha = .05, kind = "upper") {
  if (kind == "lower") {
    result <- N * (1 - stats::qbeta(1 - alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == 0] <- 0
  }
  else if (kind == "upper") {
    result <- N * (1 - stats::qbeta(alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == N] <- N[D == N]
  }

  return(result)
}

test_that("refactored verison outputs the same as the original one", {
  expect_equal(
    original_find.climits(10, 5),
    refactored_find.climits(10, 5)
  )
})
