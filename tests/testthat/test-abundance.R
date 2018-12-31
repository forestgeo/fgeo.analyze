context("abundance")

library(dplyr)

test_that("with cero-lenght dataframe returns a dataframe and result it 0", {
  expect_equal(abundance(tibble()), tibble(n = 0L))
})

describe("abundance", {
  skip_if_not_installed("ctfs")
  abun_ctfs <- function(x, ...) ctfs::abundance(x, ...)$abund$all
  tree_id <- function(x) {
    tibble(
      treeID = as.character(x),
      dbh = 1,
      status = "A",
      date = 800
    )
  }

  it("behaves as ctfs::abundance()", {
    expect_ctfs_equal_to_fgeo <- function(ids) {
      expect_equal(abun_ctfs(tree_id(ids)), abundance(tree_id(ids))$n)
    }
    expect_ctfs_equal_to_fgeo(ids = 1)
    expect_ctfs_equal_to_fgeo(ids = 1:2)

    # Grouped summaries return similar (but are implemented diferently)
    tree <- mutate(tree_id(1:2), sp = letters[1:2])
    expect_equal(
      abun_ctfs(tree, split1 = tree$sp),
      abundance(group_by(tree, sp))$n
    )
  })

  it("Returns consistent output with cero row dataframe", {
    cero_row_dfm <- tree_id(1)[0, ]
    expect_equal(abundance(cero_row_dfm)$n, 0)
    # ctfs's version resuls in dataframe with 0 rows and 0 columns
    expect_equal(abun_ctfs(cero_row_dfm), NULL)
  })

  it("works with both census and ViewFullTable", {
    expect_equal(
      abundance(tree_id(1)),
      abundance(rename(tree_id(1), TreeID = treeID, DBH = dbh))
    )
  })

  it("warns duplicated treeid", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), stemID = c("1.1", "1.2"))),
      "treeid.*Duplicated values.*Do you need to pick main stems?"
    )
  })

  it("warns multiple censusid", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), CensusID = c("1", "2"))),
      "censusid.*Multiple values.*Do you need to group by.*censusid?"
    )
  })

  it("warns multiple plotname", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), PlotName = c("a", "b"))),
      "plotname.*Multiple values.*Do you need to pick a single plot?"
    )
  })

  it("doesn't change the case of input names or groups", {
    tree <- mutate(tree_id(1:2), CensusID = 1:2)
    abund <- abundance(group_by(tree, CensusID))
    expect_named(abund, c("CensusID", "n"))
    expect_equal(group_vars(abund), "CensusID")
  })

  it("returns groups of grouped data", {
    tree <- mutate(tree_id(1:2), CensusID = 1:2)
    expect_true(is_grouped_df(abundance(group_by(tree, CensusID))))
    expect_true(is_grouped_df(basal_area(group_by(tree, CensusID))))
  })
})




context("basal_area")

describe("basal_area", {
  skip_if_not_installed("ctfs")
  ba_ctfs <- function(x, type = "ba", ...) ctfs::abundance(x, type, ...)$ba$all
  tree_id <- function(x) {
    tibble(
      treeID = as.character(x),
      dbh = 1,
      status = "A",
      date = 800
    )
  }

  it("behaves as ctfs::abundance(type = 'ba')", {
    # The reason for an offset is that Condit's funciton outputs basal area in
    # squared meters. Instead, basal_area() does not convert units so the output
    # is in the units of the input -- i.e. generally mm.
    offset <- 1000000
    # numeric
    expect_equal(basal_area(tree_id(1))$basal_area, ba_ctfs(tree_id(1)) * offset)
    # data.frame
    expect_equal(
      basal_area(tree_id(1:2))$basal_area,
      ba_ctfs(tree_id(1:2)) * offset
    )

    # Grouped summaries return similar (but are implemented diferently)
    tree <- mutate(tree_id(1:2), sp = letters[1:2])
    expect_equal(
      basal_area(group_by(tree, sp))$basal_area,
      ba_ctfs(tree, split1 = tree$sp) * offset
    )
  })

  it("warns duplicated stemid", {
    # basal_area warns not treeid but stemid
    expect_warning(
      basal_area(mutate(tree_id(c(1, 1)), stemID = c("1.1", "1.1"))),
      "stemid.*Duplicated values.*Do you need to pick largest.*hom.*values?"
    )
    expect_silent(basal_area(tree_id(c(1, 1))))
  })

  it("warns multiple plotname and censusid", {
    expect_warning(
      basal_area(mutate(tree_id(c(1, 1)), PlotName = c("a", "b"))),
      "plotname.*Multiple values.*Do you need to pick a single plot?"
    )

    expect_warning(
      basal_area(mutate(tree_id(c(1, 1)), CensusID = c("1", "2"))),
      "censusid.*Multiple values.*Do you need to group by.*censusid?"
    )
  })
})

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6,
  dbh = rnorm(6)
)

test_that("retuns a numeric vector", {
  result <- basal_area_dbl(df$dbh)
  expect_type(result, "double")
  expect_true(rlang::is_vector(result))
})

test_that("returns the expected data structure", {
  result <- basal_area(df)
  expect_type(result, "list")
  expect_true(is.data.frame(basal_area(df)))

  expect_type(basal_area_dbl(df$dbh), "double")
})

test_that("returns the correct sum", {
  df <- data.frame(
    sp = rep(letters[1:3], each = 2),
    status = rep(c("A", "D"), 3),
    quadrat = 1:6,
    dbh = rnorm(6)
  )
  df$ba <- basal_area_dbl(df$dbh)

  actual <- df %>%
    group_by(quadrat) %>%
    basal_area() %>%
    pull(basal_area) %>%
    sum()

  expected <- sum(df$ba)
  expect_equal(actual, expected)
})

test_that("weird arguments throw error", {
  expect_error(basal_area(NULL))
  expect_error(basal_area(NA))
})

test_that("tricky objects in global environment cause no scoping issues", {
  group_by <- c("status") # this should be ignored
  nms <- basal_area(df) %>%
    as_tibble() %>%
    names()
  expect_false("status" %in% nms)
})

test_that("with cero-lenght dataframe returns a dataframe and result it 0", {
  expect_equal(abundance(tibble()), tibble(n = 0L))
})
