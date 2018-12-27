#' Count and basal area of neighboring stems, optionally by groups.
#'
#' `count_neighbour()` and `basal_area_neighbour()` output the same result as
#' `NeighborDensities()` from the CTFS-R package with the argument `type =
#' "count"` and `type = "basal"`, respectively. Compared to
#' `NeighborDensities()`, the main difference is that these functions throw more
#' informative warnings and error messages, and have simpler interfaces. They
#' don't deal with dbh or status, meaning that you should pick the values you
#' want before using these functions. And they allow you to compute by groups
#' created with [dplyr::group_by()].
#'
#' @section Warning:
#' This function has a pending issue by which a dataset with a single row does
#' not result in zero conspecific neighbors
#' (<https://github.com/forestgeo/fgeo.abundance/issues/68>).
#'
#' @param .data A Dataframe; particularly  a ForestGEO tree table.
#' @param .subset An optional dataframe giving the (`gx`, `gy`) coordinates of
#'   specific individuals (`tag` and `sp`) for which to count neighbors. For
#'   example, `.subset` may be a subset of only one species; or seedling that
#'   are not part of census `.data`. `.subset` must have columns `gx`, `gy`,
#'   `sp`, and `tag`.
#' @param r Radius.
#' @param plotdim The x and y dimensions of the plot.
#'
#' @aliases abundance_neighbor
#'
#' @return A dataframe.
#'
#' A dataframe with two columns `conspecific` and `heterospecific`, and
#' optionally one extra column for each variable used to group by. The number
#' of rows is as follows:
#' * If `.subset` is `NULL`, the output and `.data` have equal number of rows,
#' both for grouped and ungrouped `.data`.
#' * If `.subset` is not `NULL`, and `.data` is ungrouped, the output and
#' `.subset` have equal number of rows.
#' * If `.subset` is not `NULL`, and `.data` is grouped, the output has a number
#' of rows that equals that of `.subset` multiplied by the number of groups.
#'
#' @examples
#' tree <- fgeo.x::download_data("luquillo_tree5_random")
#'
#' # Guess `plotdim`
#' abundance_neighbor(tree, r = 20)
#'
#' # Explicit `plotdim`
#' abundance_neighbor(tree, r = 20, plotdim = c(320, 500))
#'
#' basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
#'
#' # Notice how the number of rows of the output varies with the input -------
#'
#' tree <- tibble::tribble(
#'   ~treeID, ~stemID, ~gx, ~gy, ~tag,   ~sp,  ~dbh, ~status,
#'      "01",    "01",   5,   5, "01", "sp1",     5,     "A",
#'      "02",    "01",   5,   5, "02", "sp1",     5,     "A",
#'      "03",    "01",   5,   5, "03", "sp2",     5,     "A",
#'      "04",    "01",   5,   5, "04", "sp2",     5,     "A"
#' )
#'
#' # ungrouped `.data`, `.subset = NULL`,
#' abundance_neighbor(tree, .subset = NULL, r = 20, plotdim = c(320, 500))
#'
#' # grouped `.data`, `.subset = NULL`
#' by_sp <- dplyr::group_by(tree, sp)
#' abundance_neighbor(by_sp, .subset = NULL, r = 20, plotdim = c(320, 500))
#'
#' subset <- tibble::tribble(
#'   ~gx, ~gy, ~tag,   ~sp,
#'     3,   3, "99", "sp1"
#' )
#'
#' # ungrouped `.data`, `.subset` not `NULL`
#' abundance_neighbor(tree, .subset = subset, r = 20, plotdim = c(320, 500))
#'
#' # grouped `.data`, `.subset` not `NULL`
#' by_sp <- dplyr::group_by(tree, sp)
#' abundance_neighbor(by_sp, .subset = subset, r = 20, plotdim = c(320, 500))
#' @name neighbor
NULL

neighbor <- function(type) {
  force(type)
  function(.data,
           .subset = NULL,
           r,
           plotdim = NULL) {
    check_neighbor(.data, .subset)

    plotdim <- plotdim %||% fgeo.tool::guess_plotdim(.data)
    out <- dplyr::do(
      .data,
      # Cancel message "elapsed time ...". Argument `quiet` isn't justifyed.
      neighbours = suppressMessages(
        neighbor_densities(., .subset, r, plotdim, type = type)
      )
    )
    tidyr::unnest(out)
  }
}

#' @rdname neighbor
#' @export
abundance_neighbor <- neighbor(type = "count")

#' @rdname neighbor
#' @export
basal_area_neighbor <- neighbor(type = "basal")

check_neighbor <- function(.data, .subset) {
  stopifnot(is.data.frame(.data))
  if (!is.null(.subset)) stopifnot(is.data.frame(.subset))

  crucial_sub <- c("gx", "gy", "tag", "sp")
  crucial_data <- c(crucial_sub, "dbh", "status")
  prepend_crucial_nm_msg(.data, crucial_data, "Invalid `.data`. ")
  if (!is.null(.subset)) {
    prepend_crucial_nm_msg(.subset, crucial_sub, "Invalid `.subset`. ")
  }

  fgeo.tool::flag_if_group(.data, "treeid", is_duplicated, warn)

  invisible(.data)
}

prepend_crucial_nm_msg <- function(x, nm, msg) {
  tryCatch(
    check_crucial_names(x, nm),
    error = function(e) {
      e$message <- paste0(msg, e$message)
      stop(e)
    }
  )
}

space <- function(...) {
  paste(unlist(list(...)), collapse = " ")
}

# ctfs --------------------------------------------------------------------

#' Internal. Adapted from its original version.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
neighbor_densities <- function(.data,
  .subset,
  r,
  plotdim,
  mindbh = min(.data$dbh, na.rm = TRUE),
  type,
  include = unique(.data$status)) {
  ptm <- proc.time()

  if (is.null(.subset)) .subset <- .data
  n <- nrow(.subset)
  output <- matrix(NA, ncol = 2, nrow = n)
  dimnames(output)[[2]] <- c("conspecific", "heterospecific")

  if (type == "count") {
    count_data_is_good <-
      !is.na(.data$gx) &
      !is.na(.data$gy) &
      !duplicated(.data$tag) &
      .data$dbh >= mindbh &
      .data$status %in% include
    spd <- .data[count_data_is_good, , drop = FALSE]
    spd <- rm_na_row(spd)

    for (i in 1:n) {
      focal <- .subset[i, ]

      focal_is_bad <- is.na(focal$gx) | is.na(focal$gy) | duplicated(focal$tag)
      if (focal_is_bad) {
        output[i, 1:2] <- NA
      } else {
        around_focal <- c(
          focal$gx - r,
          focal$gy - r,
          focal$gx - r,
          focal$gy + r,
          focal$gx + r,
          focal$gy + r,
          focal$gx + r,
          focal$gy - r
        )
        poly <- splancs::spoints(around_focal)
        use <- splancs::inpip(splancs::spoints(rbind(spd$gx, spd$gy)), poly)

        if (length(use) == 0) {
          output[i, 1:2] <- 0
        } else {
          incircle <- sqrt(splancs::dsquare(
            splancs::spoints(rbind(spd$gx[use], spd$gy[use])),
            splancs::spoints(rbind(focal$gx, focal$gy))
          )) <= r

          nn <- use[incircle]
          consp <- spd$sp[nn] == focal$sp

          focal_coords <- data.frame(gx = focal$gx, gy = focal$gy)
          area <- CalcRingArea(focal_coords, r, plotdim)$each

          output[i, 1] <- length(nn[consp]) * (pi * r^2 / area)
          output[i, 2] <- length(nn[!consp]) * (pi * r^2 / area)
        }
      }

      if (i %in% seq(5000, n + 5000, 5000)) {
        message(space(i, "of", n, " elapsed time ~", elapsed(ptm), "seconds", "\n"))
      }
    }
  }
  if (type == "basal") {
    basal_data_is_good <-
      !is.na(.data$gx) &
      !is.na(.data$gy) &
      !is.na(.data$dbh) &
      .data$dbh >= mindbh &
      .data$status %in% include
    spd <- .data[basal_data_is_good, , drop = FALSE]
    spd <- rm_na_row(spd)
    for (i in 1:n) {
      focal <- .subset[i, ]
      if (is.na(focal$gx) | is.na(focal$gy)) {
        output[i, 1:2] <- NA
      } else {
        around_focal <- c(
          focal$gx - r,
          focal$gy - r,
          focal$gx - r,
          focal$gy + r,
          focal$gx + r,
          focal$gy + r,
          focal$gx + r,
          focal$gy - r
        )
        poly <- splancs::spoints(around_focal)
        use <- splancs::inpip(splancs::spoints(rbind(spd$gx, spd$gy)), poly)

        if (length(use) == 0) {
          output[i, 1:2] <- 0
        } else {
          incircle <- sqrt(
            splancs::dsquare(
              splancs::spoints(rbind(spd$gx[use], spd$gy[use])),
              splancs::spoints(rbind(focal$gx, focal$gy))
            )) <= r

          nn <- use[incircle]

          focal_coords <- data.frame(gx = focal$gx, gy = focal$gy)
          area <- CalcRingArea(focal_coords, r, plotdim)$each

          BA <- circlearea(spd$dbh[nn] / 20)
          consp <- spd$sp[nn] == focal$sp
          output[i, 1] <- sum(BA[consp], na.rm = TRUE) * (pi * r^2 / area)
          output[i, 2] <- sum(BA[!consp], na.rm = TRUE) * (pi * r^2 / area)
        }
      }
      if (i %in% seq(5000, n + 5000, 5000)) {
        message(space(i, "of", n, " elapsed time ~", elapsed(ptm), "seconds", "\n"))
      }
    }
  }
  message(space("Total elapsed time ~", elapsed(ptm), "seconds", "\n"))

  tibble::as.tibble(output)
}

elapsed <- function(ptm) {
  round((proc.time() - ptm), 3)[3]
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
CalcRingArea <- function(data, radius, plotdim) {
  nopts <- dim(data)[1]
  internalArea <- numeric()
  for (i in 1:nopts) {
    xdist <- data$gx[i]
    if (plotdim[1] - data$gx[i] < xdist) {
      xdist <- plotdim[1] - data$gx[i]
    }
    internalArea[i] <- partialcirclearea(
      radius, xdist, data$gy[i],
      plotdim[2] - data$gy[i]
    )
  }
  return(list(total = sum(internalArea), each = internalArea))
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
partialcirclearea <- function(r, c2, cy1, cy2) {
  if (cy1 <= cy2) {
    c1 <- cy1
    c3 <- cy2
  }
  else {
    c1 <- cy2
    c3 <- cy1
  }
  if (r > c1) {
    alpha1 <- acos(c1 / r)
    y1 <- sqrt(r * r - c1 * c1)
  }
  if (r > c2) {
    alpha2 <- acos(c2 / r)
    y2 <- sqrt(r * r - c2 * c2)
  }
  if (r > c3) {
    alpha3 <- acos(c3 / r)
    y3 <- sqrt(r * r - c3 * c3)
  }
  cornerdist1 <- sqrt(c1 * c1 + c2 * c2)
  cornerdist3 <- sqrt(c2 * c2 + c3 * c3)
  if (r <= c1 && r <= c2) {
    area <- circlearea(r)
  } else if (r > c1 && r <= c2 && r <= c3) {
    area <- r * r * (pi - alpha1) + c1 * y1
  } else if (r <= c1 && r > c2 && r <= c3) {
    area <- r * r * (pi - alpha2) + c2 * y2
  } else if (r > c1 && r > c2 && r <= c3) {
    if (r > cornerdist1) {
      area <- r * r * (3 * pi / 4 - alpha1 / 2 - alpha2 / 2) +
        c1 * c2 + (c1 * y1 + c2 * y2) / 2
    } else {
      area <- r * r * (pi - alpha1 - alpha2) + c1 * y1 +
        c2 * y2
    }
  }
  else if (r <= c2 && r > c1 && r > c3) {
    area <- r * r * (pi - alpha1 - alpha3) + c1 * y1 + c3 *
      y3
  } else if (r > c2 && r > c1 && r > c3) {
    if (r > cornerdist3) {
      area <- r * r * (pi - alpha1 - alpha3) / 2 + c1 * c2 +
        c2 * c3 + (c1 * y1 + c3 * y3) / 2
    } else if (r > cornerdist1 && r <= cornerdist3) {
      area <- r * r * (3 * pi / 4 - alpha1 / 2 - alpha2 / 2 -
          alpha3) + (c1 * y1 + c2 * y2) / 2 + c3 * y3 + c1 *
        c2
    } else if (r <= cornerdist1 && r <= cornerdist3) {
      area <- r * r * (pi - alpha1 - alpha2 - alpha3) +
        c1 * y1 + c2 * y2 + c3 * y3
    }
  }
  return(area)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
rm_na_row <- function(.data) {
  stopifnot(length(.data) > 0)
  stopifnot(any(is.matrix(.data) || is.data.frame(.data)))
  .data[!is_na_row(.data), ]
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
circlearea <- function(r) {
  return(pi * r^2)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
is_na_row <- function(.data) {
  stopifnot(length(.data) > 0)
  stopifnot(any(is.matrix(.data) || is.data.frame(.data)))
  is_na_vector <- function(x) all(is.na(x))
  apply(.data, 1, is_na_vector)
}

