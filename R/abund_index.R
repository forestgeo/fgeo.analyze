#' Count rows by quadrat-index.
#'
#' Count rows by quadrat-index. This is a conservative wrapper around the
#' function `abundanceperquad()` of the CTFS R Package. Its output is always
#' abundance (not basal area nor agb) and includes all available rows. If you
#' want to exclude trees of some particular dbh range you need to do it before
#' using this function.
#'
#' This function is deprecated. Better alternatives to count rows by groups are
#' available in the packages __dplyr__ and __janitor__ (see `group_by()`
#' and `count()` in __dplyr__ and `tabyl()` in __janitor__). Those alternatives
#' are better tested and considerably faster.
#'
#' @param censdata A table of plot census data.
#' @param plotdim The x and y dimensions of the plot.
#' @param gridsize Side of the square quadrat.
#'
#' @return A dataframe where each quadrat-index is a column and each species
#' is a rowname.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' abund_index(fgeo.x::tree6, plotdim = c(1000, 500), gridsize = 20)
#' @family functions for abundance and basal area
#' @noRd
abund_index <- function(censdata, plotdim, gridsize) {
  stopifnot(!missing(censdata), !missing(plotdim), !missing(gridsize))
  abundanceperquad2(
    censdata = censdata, plotdim = plotdim, gridsize = gridsize, mindbh = 0
  )$abund
}
