context("neighbor")

library(dplyr)
library(purrr)
library(tidyr)
library(fgeo.tool)

pick_top <- function(.data, var, n = 1) {
  var <- enquo(var)
  pulled <- dplyr::pull(.data, !!var)
  sorted <- sort(unique(pulled))
  if (n > 0 ) {
    to_match <- head(sorted, n)
  } else {
    to_match <- tail(sorted, abs(n))
  }
  .data[pulled %in% to_match, ]
}

luquillo_tree5_random <- fgeo.x::download_data("luquillo_tree5_random")
luquillo_stem5_random <- fgeo.x::download_data("luquillo_stem5_random")
tree <- luquillo_tree5_random[1:10, ]

describe("neighbor_*() inputs", {
  it("fails with informative messages when fed with common bad-inputs", {
    no_df <- 1
    expect_error(
      abundance_neighbor(no_df, r = 20, plotdim = c(320, 500)),
      "is not TRUE"
    )
    expect_error(
      abundance_neighbor(tree, .subset = no_df, r = 20, plotdim = c(320, 500)),
      "is not TRUE"
    )
    bad_nm <- data.frame(bad_names = 1)
    expect_error(
      abundance_neighbor(bad_nm, r = 20, plotdim = c(320, 500)),
      "Invalid.*Ensure your data set has these variables"
    )
    expect_error(
      abundance_neighbor(tree, .subset = bad_nm, r = 20, plotdim = c(320, 500)),
      "Invalid.*Ensure your data set has these variables"
    )
    expect_error(
      abundance_neighbor(tree, plotdim = c(320, 500)),
      "argument.*is missing"
    )
  })

  it("warns if detects a stem (not tree) table", {
    stem <- luquillo_stem5_random
    expect_warning(abundance_neighbor(stem, r = 20), "Flagged values")
  })
})

describe("neighbor_*() outputs", {
  skip_if_not_installed("ctfs")
  ctfs_n <- ctfs::NeighborDensities(
    tree,
    r = 20, mindbh = 0, type = "count", include = unique(tree$status)
  )
  ctfs_b <- ctfs::NeighborDensities(
    tree,
    r = 20, mindbh = 0, type = "basal", include = unique(tree$status)
  )
  it("returns the same output as in the original ctfs functoin", {
    expect_known_output(ctfs_n, "ref-NeighbourDensities_count", print = TRUE)
    expect_known_output(ctfs_b, "ref-NeighbourDensities_basal", print = TRUE)
  })

  fgeo_n <- abundance_neighbor(tree, r = 20, plotdim = c(320, 500))
  fgeo_b <- basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
  it("outputs equal to the original ctfs function", {
    expect_is(fgeo_n, "tbl")
    # Names and s3 class are intentionally different:
    # * Coercing to tibble.
    # * Removing names.
    expect_identical(unname(fgeo_n), unname(tibble::as.tibble(ctfs_n)))
    expect_is(fgeo_b, "tbl")
    expect_identical(unname(fgeo_b), unname(tibble::as.tibble(ctfs_b)))

    # With the simplest input possible
    tree <- tibble::tribble(
      ~treeID, ~stemID, ~gx, ~gy, ~tag,   ~sp,  ~dbh, ~status,
         "01",    "01",   5,   5,  "a", "sp1",     5,     "A"
    )
    ctfs_n <- ctfs::NeighborDensities(
      tree, r = 20, plotdim = c(320, 500), mindbh = min(tree$dbh, na.rm = TRUE)
    )
    fgeo_n <- abundance_neighbor(tree, r = 20, plotdim = c(320, 500))
    ngdn_n <- neighbor_densities(
      tree, .subset = NULL, r = 20, plotdim = c(320, 500), type = "count"
    )
    expect_equal(fgeo_n[[1]], ctfs_n[[1]])
    expect_equal(fgeo_n[[2]], ctfs_n[[2]])
    expect_equal(ngdn_n[[1]], ctfs_n[[1]])
    expect_equal(ngdn_n[[2]], ctfs_n[[2]])
  })

  it("outputs as many rows as .data or .subset, if .data is ungrouped", {
    .data <- tibble::tribble(
      ~treeID, ~stemID, ~gx, ~gy, ~tag,   ~sp,  ~dbh, ~status,
         "01",    "01",   5,   5,  "a", "sp1",     5,     "A",
         "02",    "01",   5,   5,  "b", "sp1",     5,     "A"
    )
    data_n <- abundance_neighbor(.data, r = 20, plotdim = c(320, 500))
    expect_is(data_n, "tbl")
    expect_equal(nrow(data_n), 2)
    expect_named(data_n, c("conspecific", "heterospecific"))

    .subset <- tibble::tribble(
      ~gx, ~gy, ~tag,   ~sp,
      3,   3,  "a", "sp1"
    )
    subset_n <- abundance_neighbor(.data, .subset, r = 20, plotdim = c(320, 500))
    expect_equal(nrow(subset_n), 1)

    .subset_far <- tibble::tribble(
      ~gx,   ~gy, ~tag,   ~sp,
      100,   100,  "a", "sp1"
    )
    subset_n <- abundance_neighbor(.data, .subset_far, r = 20, plotdim = c(320, 500))
    expect_equal(subset_n[[1]], 0)
    expect_equal(subset_n[[2]], 0)
  })

  it("if .data is grouped, outputs n rows of .data/.subset times n groups", {
    .data <- tibble::tribble(
      ~treeID, ~stemID, ~gx, ~gy, ~tag,   ~sp,  ~dbh, ~status,
         "01",    "01",   5,   5,  "a", "sp1",     5,     "A",
         "02",    "01",   5,   5,  "b", "sp1",     5,     "A",
         "03",    "01",   5,   5,  "c", "sp2",     5,     "A",
         "04",    "01",   5,   5,  "d", "sp2",     5,     "A"
    )

    by_sp <- dplyr::group_by(.data, sp)
    out <- abundance_neighbor(by_sp, r = 20, plotdim = c(320, 500))
    expect_equal(nrow(out), nrow(.data))

    .subset <- tibble::tribble(
      ~gx, ~gy, ~tag,   ~sp,
        3,   3,  "e",  "sp1"
    )
    by_sp <- dplyr::group_by(.data, sp)
    out <- abundance_neighbor(by_sp, .subset, r = 20, plotdim = c(320, 500))
    expect_equal(nrow(out), nrow(.subset) * length(unique(.data$sp)))

    .subset <- tibble::tribble(
      ~gx, ~gy, ~tag,   ~sp,
        3,   3,  "e",  "sp1",
        3,   3,  "e",  "sp1",
        3,   3,  "e",  "sp2",
        3,   3,  "e",  "sp2"
    )
    by_sp <- dplyr::group_by(.data, sp)
    out <- abundance_neighbor(by_sp, .subset, r = 20, plotdim = c(320, 500))
    expect_equal(nrow(out), nrow(.subset) * length(unique(.data$sp)))
  })

  # # I'm not sure if this is the expected output. I'm talking to Dan Jhonson.
  # it("FIXME: outputs zero consp and zero heterosp if data has one row (#68)", {
  #   tree <- tibble::tribble(
  #     ~treeID, ~stemID,  ~gx,  ~gy, ~tag,   ~sp,  ~dbh, ~status,
  #     "01",    "01",   10,   11,  "a", "sp1",     5,     "A",
  #   )
  #
  #   one_n <- abundance_neighbor(tree, r = 20, plotdim = c(320, 500))
  #   expect_equal(one_n$conspecific, 0)
  #   expect_equal(one_n$heterospecific, 0)
  #
  #   one_b <- basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
  #   expect_equal(one_b$conspecific, 0)
  #   expect_equal(one_b$heterospecific, 0)
  #
  #   tree <- tibble::tribble(
  #     ~treeID, ~stemID,  ~gx,  ~gy, ~tag,   ~sp,  ~dbh, ~status,
  #     "01",    "01",   10,   11,  "a", "sp1",     5,     "A",
  #     "02",    "01",   30,   31,  "b", "sp1",     5,     "A"
  #   )
  #
  #   two_n <- abundance_neighbor(tree, r = 20, plotdim = c(320, 500))
  #   expect_equal(two_n$conspecific, c(1, 1))
  #   expect_equal(two_n$heterospecific, c(0, 0))
  #
  #   two_b <- basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
  #   ba_1 <- basal_area_dbl(1)
  #   expect_equal(two_b$conspecific, c(ba_1, ba_1))
  #   expect_equal(two_b$heterospecific, c(0, 0))
  # })
})

describe("neighbor_*() side effects", {
  it("outputs a suppressable message (not a `cat()` like notification)", {
    expect_silent(suppressMessages(abundance_neighbor(tree, r = 20)))
  })
})

describe("neighbor_*() features", {
  it("guesses `plotdim` if it's not provided", {
    expect_message(abundance_neighbor(tree, r = 20), "Guessing: plotdim")
  })

  it("outputs no message about time", {
    expect_silent(abundance_neighbor(tree, r = 20, plotdim = c(320, 500)))
  })

  it("works with grouped dataframe", {
    it("ouputs one row per row in the input", {
      skip_if_not_installed("fgeo.misc")

      tree <- luquillo_tree5_random
      # Ungrouped
      t <- suppressMessages(abundance_neighbor(tree, r = 20))
      expect_equal(nrow(t), nrow(tree))

      smaller_quad <- pick_top(tree, quadrat, 5)

      byquad <- dplyr::group_by(smaller_quad, quadrat)
      q <- suppressMessages(abundance_neighbor(byquad, r = 20))
      expect_equal(nrow(q), nrow(byquad))
      expect_named(q, c("quadrat", names(t)))

      byspquad <- dplyr::group_by(smaller_quad, quadrat, sp)
      qs <- suppressMessages(abundance_neighbor(byspquad, r = 20))
      expect_equal(nrow(qs), nrow(byspquad))
      expect_named(qs, c("quadrat", "sp", names(t)))
      # Grouping by species makes isolates each species from all other.
      # * Expect zero hererospecific neighbors.
      expect_equal(unique(qs$heterospecific), 0)
    })
  })
})

describe("neighbor_*()", {
  quads <- pick_top(luquillo_tree5_random, quadrat, 10)
  pd <- c(320, 500)
  r <- 20
  it("abundance_neighbor() outputs equal via group_by() and split()", {
    dfs <- split(quads, quads$quadrat)
    out_split <- reduce(lapply(dfs, abundance_neighbor, r = r, plotdim = pd), rbind)

    byquad <- group_by(quads, quadrat)
    out_neigh <- abundance_neighbor(byquad, r = r, plotdim = pd)
    out_neigh <- out_neigh[c("conspecific", "heterospecific")]

    expect_equal(out_neigh, out_split)
  })

  it("basal_area_neighbor() outputs equal via group_by() and split()", {
    dfs <- split(quads, quads$quadrat)
    out_split <- reduce(lapply(dfs, abundance_neighbor, r = r, plotdim = pd), rbind)

    byquad <- group_by(quads, quadrat)
    out_neigh <- abundance_neighbor(byquad, r = r, plotdim = pd)
    out_neigh <- out_neigh[c("conspecific", "heterospecific")]

    expect_equal(out_neigh, out_split)
  })
})

