#' Torus Translation Test to determine habitat associations of tree species.
#'
#' Determine habitat-species associations with code developed by Sabrina Russo,
#' Daniel Zuleta, Matteo Detto, and Kyle Harms.
#'
#' This test only makes sense at the population level. We are interested in
#' knowing whether or not individuals of a species are aggregated on a habitat.
#' Multiple stems of an individual do not represent population level processes
#' but individual level processes. Thus, you should use data of individual trees
#' -- i.e. use a _tree_ table, and not a _stem_ table with potentially multiple
#' stems per tree.
#'
#' You should only try to determine the habitat association for sufficiently
#' abundant species. In a 50-ha plot, a minimum abundance of 50 trees/species
#' has been used.
#'
#' @param tree A dataframe; a ForestGEO _tree_ table (see details).
#' @param sp Character sting giving any number of species-names.
#' @param habitat Object giving the habitat designation for each
#'   plot partition defined by `gridsize`. See [`fgeo_habitat()`].
#' @param plotdim,gridsize Plot dimensions and gridsize. If `NULL` (default)
#'   they will be guessed, and a message will inform you of the chosen values.
#'   If the guess is wrong, you should provide the correct values manually (and
#'   check that your habitat data is correct).
#'
#' @seealso [summary.tt_lst()], [summary.tt_df()], [as_tibble()],
#'   [fgeo_habitat()].
#'
#' @return A list of matrices.
#'
#' @author Sabrina Russo, Daniel Zuleta, Matteo Detto, and Kyle Harms.
#'
#' @section Acknowledgments:
#' Nestor Engone Obiang, David Kenfack, Jennifer Baltzer, and Rutuja
#' Chitra-Tarak provided feedback. Daniel Zuleta provided guidance.
#'
#' @section Interpretation of Output:
#' * `N.Hab.1`: Count of stems of the focal species in habitat 1.
#' * `Gr.Hab.1`: Count of instances the observed relative density of the focal
#' species on habitat 1 was greater than the relative density based on the TT
#' habitat map.
#' * `Ls.Hab.1`: Count of instances the observed relative density of the focal
#' species on habitat 1 was less than the relative density based on the TT
#' habitat map.
#' * `Eq.Hab.1`: Count of instances the observed relative density of the focal
#' species on habitat 1 was equal to the relative density based on the TT
#' habitat map.
#' The sum of the `Gr.Hab.x`, `Ls.Hab.x`, and `Eq.Hab.x` columns for one habitat
#' equals the number of 20 x20 quads in the plot.
#' The `Rep.Agg.Neut` columns for each habitat indicate whether the species is
#' significantly repelled (-1), aggregated (1), or neutrally distributed (0) on
#' the habitat in question.
#'
#' The probabilities associated with the test for whether these patterns are
#' statistically significant are in the `Obs.Quantile` columns for each habitat.
#' Note that to calculate the probability for _repelled_, it is _the value
#' given_, but to calculate the probability for _aggregated_, it is _one minus
#' the value given_.
#'
#' Values of the `Obs.Quantile < 0.025` means that the species is repelled from
#' that habitat, while values of the `Obs.Quantile > 0.975` means that the
#' species is aggregated on that habitat.
#'
#' @section References:
#' Zuleta, D., Russo, S.E., Barona, A. et al. Plant Soil (2018).
#' <https://doi.org/10.1007/s11104-018-3878-0>.
#'
#' @examples
#' library(fgeo.tool)
#' assert_is_installed("fgeo.x")
#'
#' # Example data
#' tree <- fgeo.x::tree6_3species
#' elevation <- fgeo.x::elevation
#'
#' # Pick alive trees, of 10 mm or more
#' census <- filter(tree, status == "A", dbh >= 10)
#'
#' # Pick sufficiently abundant species
#' pick <- filter(dplyr::add_count(census, sp), n > 50)
#'
#' # Use your habitat data or create it from elevation data
#' habitat <- fgeo_habitat(elevation, gridsize = 20, n = 4)
#'
#' # Defaults to using all species
#' as_tibble(
#'   tt_test(census, habitat)
#' )
#'
#' Reduce(rbind, tt_test(census, habitat))
#'
#' some_species <- c("CASARB", "PREMON")
#' result <- tt_test(census, habitat, sp = some_species)
#' summary(result)
#' @family habitat functions
#' @export
tt_test <- function(tree,
                    habitat,
                    sp = NULL,
                    plotdim = NULL,
                    gridsize = NULL) {
  stopifnot(is.data.frame(tree))

  n_row <- nrow(dplyr::filter_all(tree, dplyr::all_vars(is.na(.))))
  if (n_row > 0) {
    warn(glue("Dropping {n_row} row(s) full of missing values"))
    tree <- dplyr::filter_all(tree, dplyr::any_vars(!is.na(.)))
  }

  stopifnot(is.data.frame(habitat))
  if (!inherits(habitat, "fgeo_habitat")) {
    warn(glue("
      `habitat` isn't of class 'fgeo_habitat'. This commonly causes errors.
      See ?fgeo_habitat().
    "))
  }

  plotdim <- plotdim %||% fgeo.tool::extract_plotdim(habitat)
  gridsize <- gridsize %||% fgeo.tool::extract_gridsize(habitat)
  inform_gridsize_plotdim(gridsize, plotdim)

  sp <- sp %||% unique(tree$sp)[!is.na(unique(tree$sp))]
  habitat <- sanitize_habitat_names_if_necessary(habitat)
  check_tt_test(tree, habitat, sp, plotdim, gridsize)

  abundance <- abund_index(tree, plotdim, gridsize)
  result <- lapply(
    X = sp,
    FUN = torusonesp.all,
    allabund20 = abundance,
    hab.index20 = habitat,
    plotdim = plotdim,
    gridsize = gridsize
  )

  new_tt_lst(result)
}

abort_if_zero_tortotstcnthab <- function() {
  abort(glue("
    Every quadrat in the dataset must have at lease one tree.
    For more information see https://github.com/forestgeo/fgeo.analyze/issues/40.
  "))
}

torusonesp.all <- function(species, hab.index20, allabund20, plotdim, gridsize) {
  stopifnot(!is.na(species), !is.null(species))

  # Calculates no. of x-axis quadrats of plot. (x is the long axis of plot in
  # the case of Pasoh)
  plotdimqx <- plotdim[1] / gridsize
  # Calculates no. of y-axis quadrats of plot.
  plotdimqy <- plotdim[2] / gridsize
  # Determines tot. no. of habitat types.
  num.habs <- length(unique(hab.index20$habitats))

  GrLsEq <- matrix(0, 1, num.habs * 6) # Creates empty matrix for output.
  rownames(GrLsEq) <- species # Names single row of output matrix.

  # Creates names for columns of output matrix.
  for (i in 1:num.habs) {
    if (i == 1) {
      cols <- c(
        paste("N.Hab.", i, sep = ""),
        paste("Gr.Hab.", i, sep = ""),
        paste("Ls.Hab.", i, sep = ""),
        paste("Eq.Hab.", i, sep = ""),
        paste("Rep.Agg.Neut.", i, sep = ""),
        paste("Obs.Quantile.", i, sep = "")
      )
    }
    if (i > 1) {
      cols <- c(
        cols,
        paste("N.Hab.", i, sep = ""),
        paste("Gr.Hab.", i, sep = ""),
        paste("Ls.Hab.", i, sep = ""),
        paste("Eq.Hab.", i, sep = ""),
        paste("Rep.Agg.Neut.", i, sep = ""),
        paste("Obs.Quantile.", i, sep = "")
      )
    }
  }
  # Names columns of output matrix.
  colnames(GrLsEq) <- cols

  # CALCULATIONS FOR OBSERVED RELATIVE DENSITIES ON THE TRUE HABITAT MAP

  # pulls out the abundance by quad data for the focal species
  allabund20.sp <- allabund20[which(rownames(allabund20) == species), ]
  # Fills a matrix, with no. rows = plotdimqy (dim 2) and no. columns =
  # plotdimqx (dim 1), with the indiv. counts per quadrat of one species.
  spmat <- matrix(
    as.numeric(allabund20.sp),
    nrow = plotdimqy, plotdimqx, byrow = FALSE
  )
  # calculates total number of stems in each quad for all species and puts in
  # matrix
  totmat <- matrix(
    apply(allabund20, MARGIN = 2, FUN = "sum"),
    plotdimqy, plotdimqx,
    byrow = FALSE
  )

  # fills matrix with habitat types, oriented in the same way as the species and
  # total matrices above
  habmat <- matrix(
    hab.index20$habitats,
    nrow = plotdimqy, ncol = plotdimqx, byrow = FALSE
  )

  # FIXME: Super slow. Should be as long as the output
  # Creates empty vector for stem counts per sp. per habitat.
  spstcnthab <- numeric()
  # FIXME: Super slow. Should be as long as the output
  # Creates empty vector for tot. stem counts per habitat.
  totstcnthab <- numeric()

  for (i in 1:num.habs) {
    # Determines tot. no. stems per habitat of the true map.
    totstcnthab[i] <- sum(totmat[habmat == i])
    # Determines tot. no. stems for focal sp. per habitat of the true map.
    spstcnthab[i] <- sum(spmat[habmat == i])
  }

  # Calculates observed relative stem density of focal sp. per habitat of the
  # true map.
  spprophab <- spstcnthab / totstcnthab

  # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS
  habmat.template <- habmat

  for (j in 1:4) {
    # apply rotations and mirrors
    # if j==1 do nothing

    if (j == 2) habmat <- apply(habmat.template, 2, rev)
    if (j == 3) habmat <- t(apply(habmat.template, 1, rev))
    if (j == 4) habmat <- t(apply(apply(habmat.template, 2, rev), 1, rev))


    # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS

    # Opens "for loop" through all 20-m translations along x-axis.
    for (x in 0:(plotdimqx - 1)) {
      # Opens "for loop" through all 20-m translations along y-axis.
      for (y in 0:(plotdimqy - 1)) {
        # FIXME: Super slow. Should be as long as the output
        # Creates empty matrix of quadrats' habitat designations.
        newhab <- matrix(0, plotdimqy, plotdimqx)

        # The following "if" statements create the x,y torus-translation of the
        # habitat map.
        if (y == 0 & x == 0) {
          newhab <- habmat
        }

        if (y == 0 & x > 0) {
          newhab <- habmat[
            c(1:plotdimqy),
            c((plotdimqx - x + 1):plotdimqx, 1:(plotdimqx - x))
          ]
        }

        if (x == 0 & y > 0) {
          newhab <- habmat[
            c((plotdimqy - y + 1):plotdimqy, 1:(plotdimqy - y)),
            c(1:plotdimqx)
          ]
        }

        if (x > 0 & y > 0) {
          newhab <- habmat[
            # TODO: DRY
            c((plotdimqy - y + 1):plotdimqy, 1:(plotdimqy - y)),
            # TODO: DRY
            c((plotdimqx - x + 1):plotdimqx, 1:(plotdimqx - x))
          ]
        }

        # FIXME: Super slow. Should be as long as the output
        # Creates empty vector for stem counts per sp. per habitat in
        # torus-based maps.
        Torspstcnthab <- numeric()
        # Creates empty vector for tot. stem counts per habitat in torus-based
        # maps.
        Tortotstcnthab <- numeric()

        for (i in 1:num.habs) {
          # Determines tot. no. stems per habitat of the focal torus-based map.
          Tortotstcnthab[i] <- sum(totmat[newhab == i])
          # Determines tot. no. stems for focal sp. per habitat of the focal
          # torus-based map.
          Torspstcnthab[i] <- sum(spmat[newhab == i])
        }

        # Calculates relative stem density of focal sp. per habitat of the focal
        # torus-based map.
        Torspprophab <- Torspstcnthab / Tortotstcnthab
        if (any(Tortotstcnthab == 0)) {
          abort_if_zero_tortotstcnthab()
        }

        for (i in 1:num.habs) {
          # FIXME: May be NA?
          if (spprophab[i] > Torspprophab[i]) {
            # If rel. dens. of focal sp. in focal habitat of true map is greater
            # than rel. dens. of focal sp. in focal habitat of torus-based map,
            # then add one to "greater than" count.
            GrLsEq[1, (6 * i) - 4] <- GrLsEq[1, (6 * i) - 4] + 1
          }

          if (spprophab[i] < Torspprophab[i]) {
            # If rel. dens. of focal sp. in focal habitat of true map is less
            # than rel. dens. of focal sp. in focal habitat of torus-based map,
            # then add one to "less than" count.
            GrLsEq[1, (6 * i) - 3] <- GrLsEq[1, (6 * i) - 3] + 1
          }

          if (spprophab[i] == Torspprophab[i]) {
            # If rel. dens. of focal sp. in focal habitat of true map is equal
            # to rel. dens. of focal sp. in focal habitat of torus-based map,
            # then add one to "equal to" count.
            GrLsEq[1, (6 * i) - 2] <- GrLsEq[1, (6 * i) - 2] + 1
          }
        }
      } # Closes "for loop" through all 20-m translations along x-axis.
    } # Closes "for loop" through all 20-m translations along y-axis.
  } # Closes for loop through mirrors and rotations (j)


  for (i in 1:num.habs) {
    # add counts of No. stems in each habitat
    GrLsEq[1, (6 * i) - 5] <- spstcnthab[i]

    # if rel.dens. of sp in true map is greater than rel. dens. in torus map
    # less than 2.5% of the time, then repelled
    if (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) <= 0.025) {
      GrLsEq[1, (6 * i) - 1] <- -1
    }

    # if rel.dens. of sp in true map is greater than rel. dens. in torus map
    # more than 97.5% of the time, then aggregated
    if (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) >= 0.975) {
      GrLsEq[1, (6 * i) - 1] <- 1
    }

    # otherwise it's neutral (not different from random dist)
    if ((GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) < 0.975) &
      (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) > 0.025)) {
      GrLsEq[1, (6 * i) - 1] <- 0
    }

    # quantile in the TT distribtution of relative densities of the true
    # relative density
    GrLsEq[1, (6 * i)] <- GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy))
  }

  GrLsEq
}

sanitize_habitat_names_if_necessary <- function(habitat) {
  tryCatch(
    check_crucial_names(habitat, c("x", "y")),
    error = function(e) rename_to_xy(habitat)
  )
}

inform_gridsize_plotdim <- function(gridsize, plotdim) {
  hint <- "To change this value see `?tt_test()`."
  inform(glue("Using `plotdim = c({commas(plotdim)})`. {hint}"))
  inform(glue("Using `gridsize = {gridsize}`. {hint}"))
}

rename_to_xy <- function(x) {
  .x <- x
  .x <- fgeo.tool::nms_try_rename(.x, want = "x", try = "gx")
  .x <- fgeo.tool::nms_try_rename(.x, want = "y", try = "gy")
  .x
}

check_tt_test <- function(census, habitat, sp, plotdim, gridsize) {
  tree_names <- c(
  "treeID", "stemID", "tag", "StemTag", "sp", "quadrat", "gx", "gy",
  "MeasureID", "CensusID", "dbh", "pom", "hom", "ExactDate", "DFstatus",
  "codes", "nostems", "status", "date"
  )
  has_stem_names <- !all(names(census) %in% tree_names)

  msg <- "Is `census` a tree table (not a stem table)? See `?tt_test()`."
  if (has_stem_names) warn(msg)

  if (!any(is.character(sp) || is.factor(sp))) {
    msg <- paste0(
      "`sp` must be of class character or factor but is of class ",
      class(sp), "."
    )
    rlang::abort(msg)
  }

  valid_sp <- sp %in% unique(census$sp)[!is.na(unique(census$sp))]
  if (!all(valid_sp)) {
    msg <- paste0(
      "All `sp` must be present in `census`.\n",
      "Odd: ", commas(sp[!valid_sp])
    )
    abort(msg)
  }

  stopifnot(
    is.numeric(plotdim),
    length(plotdim) == 2,
    is.numeric(gridsize),
    length(gridsize) == 1
  )
  common_gridsize <- gridsize %in% c(5, 10, 20)
  if (!common_gridsize) {
    rlang::warn(paste("Uncommon `gridsize`:", gridsize, "\nIs this expected?"))
  }
}

# Warns that a comparison is invalid. Results from a division `NaN = 0/0`
warn_invalid_comparison <- function(spp, torus) {
  msg <- "Values can't be compared:\n"
  value <- paste0(
    "spprophab = ", spp, " vs. ",
    "Torspprophab = ", torus, "\n"
  )
  rlang::warn(paste0(msg, value))
}

new_tt_lst <- function(.x) {
  stopifnot(is.list(.x))
  structure(.x, class = c("tt_lst", class(.x)))
}

#' @export
print.tt_lst <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
