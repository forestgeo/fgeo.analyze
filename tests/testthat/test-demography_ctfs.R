context("demography_ctfs")

library(dplyr)
library(purrr)

expect_ref <- function(object, file) {
  testthat::expect_known_output(object, file, print = TRUE, update = FALSE)
}

pick10sp <- function(.data) dplyr::filter(.data, sp %in% unique(.data$sp)[1:10])
pluck_n <- function(.x, n) lapply(.x, function(x) x[1:n])

tiny1 <- fgeo.x::tree5
tiny2 <- fgeo.x::tree6

test_that("output is equal if aggregated via `split2` or `interaction()`", {
  # Toy groups
  tiny1$g1 <- sample(c("a", "b"), nrow(tiny1), replace = TRUE)
  tiny1$g2 <- sample(1:2, nrow(tiny1), replace = TRUE)

  # Suppress "This warning is displayed once per session."
  spl <- suppressWarnings(
    recruitment_ctfs(tiny1, tiny2, split1 = tiny1$g1, split2 = tiny1$g2)
  )
  int <- recruitment_ctfs(tiny1, tiny2, split1 = interaction(tiny1$g1, tiny1$g2))
  expect_equal(
    map(spl, as.vector),
    map(int, as.vector)
  )
})

test_that("output is equal if aggregated via `split2` or `interaction()`", {
  skip("FIXME: Real groups don't produce the same output")

  spl <- recruitment_ctfs(
    tiny1, tiny2,
    split1 = tiny1$sp, split2 = tiny1$quadrat
  )
  int <- recruitment_ctfs(
    tiny1, tiny2,
    split1 = interaction(tiny1$sp, tiny1$quadrat)
  )
  expect_equal(
    map(map(spl, as.vector), head),
    map(map(int, as.vector), head)
  )
})

test_that("objects created with split2 have split2 attribute", {
  out <- recruitment_ctfs(
    tiny1, tiny2,
    split1 = tiny1$sp, split2 = tiny1$quadrat
  )
  expect_true(attr(out, "split2"))
})

test_that("without split2, doesn't have attribute split2", {
  out <- recruitment_ctfs(tiny1, tiny2)
  expect_null(attr(out, "split2"))
})

test_that("using `mindbh` prints a message after extracting desired `mindbh`", {
  expect_message(
    recruitment_ctfs(
      pick10sp(fgeo.x::tree5), pick10sp(fgeo.x::tree6),
      mindbh = 100
    ),
    "Using dbh.*and above"
  )
})

test_that("growth_ctfs() outputs differently with different `method`s", {
  i <- growth_ctfs(tiny1, tiny2, method = "I")
  e <- growth_ctfs(tiny1, tiny2, method = "E")
  expect_false(identical(i, e))
})

describe("recruitment_ctfs(), mortality_ctfs(), and growth_ctfs()", {
  it("output S3 objects of class demography_ctfs", {
    out1 <- recruitment_ctfs(tiny1, tiny2, quiet = TRUE)
    expect_is(out1, "demography_ctfs")

    out1 <- mortality_ctfs(tiny1, tiny2, quiet = TRUE)
    expect_is(out1, "demography_ctfs")

    out1 <- growth_ctfs(tiny1, tiny2, quiet = TRUE)
    expect_is(out1, "demography_ctfs")

    out1 <- recruitment_ctfs(
      tiny1, tiny2,
      split1 = tiny1$sp, quiet = TRUE
    )
    expect_is(out1, "demography_ctfs")
    out1 <- mortality_ctfs(
      tiny1, tiny2,
      split1 = tiny1$sp, quiet = TRUE
    )
    expect_is(out1, "demography_ctfs")
    out1 <- growth_ctfs(
      tiny1, tiny2,
      split1 = tiny1$sp, quiet = TRUE
    )
    expect_is(out1, "demography_ctfs")
  })

  it("output equivalent to ctfs analogs (different class)", {
    skip_if_not_installed("ctfs")

    out1 <- recruitment_ctfs(
      tiny1, tiny2,
      split1 = tiny1$sp, quiet = TRUE
    )
    expect_is(out1, "demography_ctfs")
    out2 <- ctfs::recruitment(tiny1, tiny2, split1 = tiny1$sp)
    expect_equivalent(out1, out2)

    out1 <- mortality_ctfs(
      tiny1, tiny2,
      split1 = tiny1$sp, quiet = TRUE
    )
    out2 <- ctfs::mortality(tiny1, tiny2, split1 = tiny1$sp)
    expect_equivalent(out1, out2)

    # Bug in ctfs::growth(); fixed in growth_ctfs().
    # Works
    expect_error(
      growth_ctfs(tiny1, tiny2, split1 = tiny1$sp, quiet = TRUE), NA
    )
    # Fails
    expect_error(ctfs::growth(tiny1, tiny2, split1 = tiny1$sp))
  })

  it("outputs an OK data structure", {
    r_luq_t <- recruitment_ctfs(tiny1, tiny2)
    expect_ref(r_luq_t, "ref-recruitment_ctfs_luq_tree")
    expect_type(r_luq_t, "list")
    expect_is(r_luq_t[[1]], "numeric")
    expect_length(r_luq_t, 8)
    expect_false(any(is.na(r_luq_t)))

    m_luq_t <- mortality_ctfs(tiny1, tiny2)
    expect_ref(m_luq_t, "ref-mortality_ctfs_luq_tree")
    expect_type(m_luq_t, "list")
    expect_is(m_luq_t[[1]], "numeric")
    expect_length(m_luq_t, 9)
    expect_false(any(is.na(m_luq_t)))

    g_luq_t <- growth_ctfs(tiny1, tiny2)
    expect_ref(g_luq_t, "ref-growth_ctfs_luq_tree")
    expect_type(m_luq_t, "list")
    expect_is(g_luq_t[[1]], "numeric")
    expect_length(g_luq_t, 7)
    expect_false(any(is.na(g_luq_t)))
  })

  it("errs if crucial variables are missing", {
    expect_error(recruitment_ctfs(rename(tiny1, bad = dbh), tiny2), "Ensure")
    expect_error(recruitment_ctfs(rename(tiny1, bad = hom), tiny2), "Ensure")
    expect_error(recruitment_ctfs(rename(tiny1, bad = status), tiny2), "Ensure")
    expect_error(recruitment_ctfs(rename(tiny1, bad = date), tiny2), "Ensure")

    expect_error(recruitment_ctfs(tiny1, rename(tiny2, bad = dbh)), "Ensure")
    expect_error(recruitment_ctfs(tiny1, rename(tiny2, bad = hom)), "Ensure")
    expect_error(recruitment_ctfs(tiny1, rename(tiny2, bad = status)), "Ensure")
    expect_error(recruitment_ctfs(tiny1, rename(tiny2, bad = date)), "Ensure")

    expect_error(mortality_ctfs(rename(tiny1, bad = dbh), tiny2), "Ensure")
    expect_error(mortality_ctfs(rename(tiny1, bad = hom), tiny2), "Ensure")
    expect_error(mortality_ctfs(rename(tiny1, bad = status), tiny2), "Ensure")
    expect_error(mortality_ctfs(rename(tiny1, bad = date), tiny2), "Ensure")

    expect_error(mortality_ctfs(tiny1, rename(tiny2, bad = dbh)), "Ensure")
    expect_error(mortality_ctfs(tiny1, rename(tiny2, bad = hom)), "Ensure")
    expect_error(mortality_ctfs(tiny1, rename(tiny2, bad = status)), "Ensure")
    expect_error(mortality_ctfs(tiny1, rename(tiny2, bad = date)), "Ensure")

    expect_error(growth_ctfs(rename(tiny1, bad = hom), tiny2), "Ensure")
    expect_error(growth_ctfs(rename(tiny1, bad = dbh), tiny2), "Ensure")
    expect_error(growth_ctfs(rename(tiny1, bad = status), tiny2), "Ensure")
    expect_error(growth_ctfs(rename(tiny1, bad = date), tiny2), "Ensure")
    expect_error(growth_ctfs(rename(tiny1, bad = stemID), tiny2), "Ensure")

    expect_error(growth_ctfs(tiny1, rename(tiny2, bad = dbh)), "Ensure")
    expect_error(growth_ctfs(tiny1, rename(tiny2, bad = hom)), "Ensure")
    expect_error(growth_ctfs(tiny1, rename(tiny2, bad = status)), "Ensure")
    expect_error(growth_ctfs(tiny1, rename(tiny2, bad = date)), "Ensure")
    # Not in recruitment_ctfs() or mortality_ctfs()
    expect_error(growth_ctfs(tiny1, rename(tiny2, bad = stemID)), "Ensure")
  })

  it("errs with informative message if censuses are missing", {
    expect_error(recruitment_ctfs(), "is missing, with no default")

    expect_error(mortality_ctfs(), "is missing, with no default")

    expect_error(growth_ctfs(), "is missing, with no default")
  })

  it("informs that `alivecode` is deprecated, but only once", {
    expect_message(
      recruitment_ctfs(tiny1, tiny2, alivecode = "A"),
      "is deprecated"
    )

    output <- capture.output(
      recruitment_ctfs(tiny1, tiny2, alivecode = "A"),
      type = "message"
    )
    expect_false(any(grepl("alivecode.*deprecated", output)))

    # Remain quiet with other demography functions
    expect_message(mortality_ctfs(tiny1, tiny2, alivecode = "A"))

    # Not applicable for growth_ctfs()
    expect_error(growth_ctfs(tiny1, tiny2, alivecode = "A"), "unused argument")
  })

  it("is sensitive to `alivecode`", {
    outA <- recruitment_ctfs(tiny1, tiny2, alivecode = "A")
    outD <- recruitment_ctfs(tiny1, tiny2, alivecode = "D")
    expect_warning(
      outBAD <- recruitment_ctfs(tiny1, tiny2, alivecode = "BAD"),
      "`alivecode` matches no value of `status`"
    )
    expect_false(identical(outA, outD))
    expect_true(all(is.na(outBAD)))

    outA <- mortality_ctfs(tiny1, tiny2, alivecode = "A")
    outD <- mortality_ctfs(tiny1, tiny2, alivecode = "D")
    expect_warning(
      outBAD <- mortality_ctfs(tiny1, tiny2, alivecode = "BAD"),
      "`alivecode` matches no value of `status`"
    )
    expect_false(identical(outA, outD))
    infinite <- is.infinite(unlist(outBAD))
    na <- is.na(unlist(outBAD))
    expect_true(any(infinite) || any(na))

    # Not applicable for growth_ctfs()
  })

  it("informs dbh range if quiet = FALSE", {
    expect_message(recruitment_ctfs(tiny1, tiny2), "Detected dbh ranges")
    expect_silent(recruitment_ctfs(tiny1, tiny2, quiet = TRUE))

    expect_message(mortality_ctfs(tiny1, tiny2), "Detected dbh ranges")
    expect_silent(mortality_ctfs(tiny1, tiny2, quiet = TRUE))

    expect_message(growth_ctfs(tiny1, tiny2), "Detected dbh ranges")
    expect_silent(growth_ctfs(tiny1, tiny2, quiet = TRUE))
  })

  it("warns if time difference is cero", {
    expect_warning(recruitment_ctfs(tiny1, tiny1), "Time difference is cero")

    expect_warning(mortality_ctfs(tiny1, tiny1), "Time difference is cero")

    expect_warning(growth_ctfs(tiny1, tiny1), "Time difference is cero")
  })

  it("errs if all dates are missing", {
    tiny1na <- tiny1
    tiny2na <- tiny2
    tiny1na$date <- NA
    tiny2na$date <- NA

    msg <- "Can't use `date`; all values are all missing."
    expect_error(growth_ctfs(tiny1na, tiny1na), msg)
    expect_error(mortality_ctfs(tiny1na, tiny1na), msg)
    expect_error(recruitment_ctfs(tiny1na, tiny1na), msg)
  })

  it("defaults to mindbh = 0", {
    out <- recruitment_ctfs(tiny1, tiny2)
    out0 <- recruitment_ctfs(tiny1, tiny2, mindbh = 0)
    expect_equal(out, out0)

    # Not applicable to mortality_ctfs()

    out <- growth_ctfs(tiny1, tiny2)
    out0 <- growth_ctfs(tiny1, tiny2, mindbh = 0)
    expect_equal(out, out0)
  })

  it("is sensitive to changing dbh", {
    out0 <- recruitment_ctfs(tiny1, tiny2, mindbh = 0)
    out100 <- recruitment_ctfs(tiny1, tiny2, mindbh = 100)
    expect_false(identical(out0, out100))

    # Not applicable to mortalit()

    out0 <- growth_ctfs(tiny1, tiny2, mindbh = 0)
    out100 <- growth_ctfs(tiny1, tiny2, mindbh = 100)
    expect_false(identical(out0, out100))
  })

  it("works with `split1`", {
    out1 <- recruitment_ctfs(tiny1, tiny2, split1 = tiny1$sp)
    expect_ref(out1, "ref-recruitment_ctfs_luq_tree_split1")
    # Output has the expected structure
    expect_type(out1, "list")
    expect_is(out1[[1]], "numeric")
    expect_length(out1, 8)
    expect_false(any(is.na(out1)))
    # Spliting by sp of census 1 or census 2 is the same
    out2 <- recruitment_ctfs(tiny1, tiny2, split1 = tiny2$sp)
    expect_true(identical(out1, out2))
    # and the result is different than not splitting at all
    out <- recruitment_ctfs(tiny1, tiny2)
    expect_false(identical(out, out2))

    out1 <- mortality_ctfs(tiny1, tiny2, split1 = tiny1$sp)
    expect_ref(out1, "ref-mortality_ctfs_luq_tree_split1")
    # Output has the expected structure
    expect_type(out1, "list")
    expect_is(out1[[1]], "numeric")
    expect_length(out1, 9)
    expect_false(any(is.na(out1)))
    # Spliting by sp of census 1 or census 2 is the same
    out2 <- mortality_ctfs(tiny1, tiny2, split1 = tiny2$sp)
    expect_true(identical(out1, out2))
    # and the result is different than not splitting at all
    out <- mortality_ctfs(tiny1, tiny2)
    expect_false(identical(out, out2))

    out1 <- growth_ctfs(tiny1, tiny2, split1 = tiny1$sp)
    expect_ref(out1, "ref-growth_ctfs_luq_tree_split1")
    # Output has the expected structure
    expect_type(out1, "list")
    expect_is(out1[[1]], "numeric")
    expect_length(out1, 7)
    expect_false(any(is.na(out1)))
    # Spliting by sp of census 1 or census 2 is the same
    out2 <- growth_ctfs(tiny1, tiny2, split1 = tiny2$sp)
    expect_true(identical(out1, out2))
    # and the result is different than not splitting at all
    out <- growth_ctfs(tiny1, tiny2)
    expect_false(identical(out, out2))
  })



  it("works with two splitting criteria", {
    out <-
      recruitment_ctfs(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
    expect_ref(pluck_n(out, 100), "ref-recruitment_ctfs_luq_tree_split2")
    # Returns a list
    expect_type(out, "list")
    # but each element is no longer numeric but matrix
    expect_is(out[[1]], "matrix")

    out <-
      mortality_ctfs(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
    expect_ref(pluck_n(out, 100), "ref-mortality_ctfs_luq_tree_split2")
    # Returns a list
    expect_type(out, "list")
    # but each element is no longer numeric but matrix
    expect_is(out[[1]], "matrix")

    out <-
      growth_ctfs(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
    expect_ref(pluck_n(out, 100), "ref-growth_ctfs_luq_tree_split2")
    # Returns a list
    expect_type(out, "list")
    # but each element is no longer numeric but matrix
    expect_is(out[[1]], "matrix")
  })

  it("works with stem tables", {
    stem5 <- fgeo.x::stem5
    stem6 <- fgeo.x::stem6

    out <- recruitment_ctfs(stem5, stem6)
    expect_ref(out, "ref-recruitment_ctfs_luq_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 8)
    expect_false(any(is.na(out)))

    out <- mortality_ctfs(stem5, stem6)
    expect_ref(out, "ref-mortality_ctfs_luq_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 9)
    expect_false(any(is.na(out)))

    out <- growth_ctfs(stem5, stem6)
    expect_ref(out, "ref-growth_ctfs_luq_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 7)
    expect_false(any(is.na(out)))
  })

  it("works with data from bci", {
    skip_if_not_installed("bciex")
    stem5 <- pick10sp(bciex::bci12s5mini)
    stem6 <- pick10sp(bciex::bci12s6mini)

    out <- recruitment_ctfs(stem5, stem6)
    expect_ref(out, "ref-recruitment_ctfs_bci_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 8)
    expect_false(any(is.na(out)))

    out <- mortality_ctfs(stem5, stem6)
    expect_ref(out, "ref-mortality_ctfs_bci_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 9)
    expect_false(any(is.na(out)))

    out <- growth_ctfs(stem5, stem6)
    expect_ref(out, "ref-growth_ctfs_bci_stem")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 7)
    expect_false(any(is.na(out)))



    tree5 <- pick10sp(bciex::bci12t5mini)
    tree6 <- pick10sp(bciex::bci12t6mini)

    out <- recruitment_ctfs(tree5, tree6)
    expect_ref(out, "ref-recruitment_ctfs_bci_tree")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 8)
    expect_false(any(is.na(out)))

    out <- mortality_ctfs(tree5, tree6)
    expect_ref(out, "ref-mortality_ctfs_bci_tree")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 9)
    expect_false(any(is.na(out)))

    out <- growth_ctfs(tree5, tree6)
    expect_ref(out, "ref-growth_ctfs_bci_tree")
    expect_type(out, "list")
    expect_is(out[[1]], "numeric")
    expect_length(out, 7)
    expect_false(any(is.na(out)))
  })
})

test_that("growth() is sensitive to `roundown`", {
  expect_error(out1 <- growth_ctfs(tiny1, tiny2, rounddown = TRUE), NA)
  expect_error(out2 <- growth_ctfs(tiny1, tiny2, rounddown = FALSE), NA)
  expect_false(identical(out1, out2))
})

test_that("growth() can handle NULL `census2$codes`", {
  # It's unclear why this is not available for census2
  expect_error(growth_ctfs(tiny1, tiny2[setdiff(names(tiny2), "codes")]), NA)
})

# Helpers -----------------------------------------------------------------

test_that("mortality.calculation is type unstable which should be avoided", {
  .scalar <- 1
  expect_is(mortality.calculation(.scalar, .scalar, 1), "data.frame")
  .matrix <- matrix(1)
  expect_is(mortality.calculation(.matrix, .matrix, 1), "list")
})
