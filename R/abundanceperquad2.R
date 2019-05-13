#' A version of ctfs::abundanceperquad() focused only for abundance.
#'
#' @inheritParams ctfs::abundanceperquad
#' @param type "abund". Other options are intentionally not possible.
#'
#' @author Richard Condit, Suzanne Lao.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @family functions for abundance and basal area
#' @noRd
abundanceperquad2 <- function(censdata,
                              mindbh = 10,
                              plotdim = c(1000, 500),
                              gridsize = 100,
                              type = "abund",
                              dbhunit = "mm") {
  sp <- censdata$sp
  quadno <- fgeo.tool::gxgy_to_index(censdata$gx, censdata$gy,
    gridsize = gridsize,
    plotdim = plotdim
  )
  result <- abundance2(censdata,
    type = type, mindbh = mindbh,
    dbhunit = dbhunit, split1 = sp, split2 = quadno
  )
  allspp <- unique(censdata$sp)
  maxquad <- floor(plotdim[1] / gridsize) * floor(plotdim[2] / gridsize)
  allquad <- 1:maxquad
  if (dim(result[[type]])[1] < length(allspp) |
    dim(result[[type]])[2] < length(allquad)) {
    result[[type]] <- fill.dimension(
      result[[type]],
      class1 = allspp,
      class2 = allquad, fill = 0
    )
  }

  result
}

#' A faster version of ctfs::abundance(), only for counts (not ba or agb).
#'
#' @author Richard Condit, Suzanne Lao.
#'
#' @family functions for abundance and basal area
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
abundance2 <- function(censdata,
                       type = "abund",
                       alivecode = c("A"),
                       mindbh = NULL,
                       dbhunit = "mm",
                       split1 = NULL,
                       split2 = NULL) {
  if (!equal(type, "abund")) {
    abort(glue("
      `type` must be 'abund'; other types are deprecated.
      Maybe you want `abundance()` of the original CTFSRPackage?
    "))
  }

  if (is.null(split1)) {
    split1 <- rep("all", dim(censdata)[1])
  }

  if (is.null(split2)) {
    split2 <- rep("all", dim(censdata)[1])
  }

  if (!is.null(mindbh)) {
    inc <- censdata$dbh >= mindbh
  } else {
    inc <- rep(TRUE, length(censdata$dbh))
  }

  alive <- rep(FALSE, length(censdata$dbh))
  for (i in seq_along(alivecode)) {
    alive[censdata$status == alivecode[i]] <- TRUE
  }

  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))
  groupvar <- list(split1[alive & inc], split2[alive & inc])
  abund <- tapply(censdata$dbh[alive & inc], groupvar, length)
  abund <- fill.dimension(abund, class1, class2)
  result <- list(abund = abund)

  result
}
