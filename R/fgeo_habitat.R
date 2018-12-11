#' Structure habitat data from measures of topography.
#'
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat (via
#' [fgeo.tool::fgeo_topography()])).
#' 2. It calculates habitats based on hierarchical clustering of the topographic
#' metrics from step 1 (via [stats::hclust()]).
#'
#' @inheritSection fgeo.tool::fgeo_elevation Input
#'
#' @author Rick Condit.
#'
#' @inheritParams fgeo.tool::fgeo_topography
#'
#' @param n Integer. Number of cluster-groups to construct (passed to the
#'   argument `k` to [stats::cutree()]).
#' @param ... Arguments passed to [fgeo.tool::fgeo_topography()].
#'
#' @seealso [fgeo.map::autoplot.fgeo_habitat()], [fgeo.tool::fgeo_topography()].
#' @family functions to construct fgeo classes
#'
#' @return A dataframe of subclass fgeo_habitat, with columns `gx` and `gy`,
#'   rounded with accuracy determined by `gridsize`, and column `habitats`, with
#'   as many distinct integer values as determined by the argument `n`.
#'
#'
#' @export
#'
#' @examples
#' # Input a ForestGEO-like elevation list
#' elev_list <- fgeo.x::elevation
#' habs <- fgeo_habitat(elev_list, n = 4, gridsize = 20)
#' str(habs)
#'
#' \dontrun{
#' installed <- requireNamespace("fgeo.map", quietly = TRUE)
#' if (installed) {
#'   library(fgeo.map)
#'   autoplot(habs)
#' }
#'
#' # A good use of habitat data is for calculating species-habitat associations
#' installed <- requireNamespace("fgeo.analyze", quietly = TRUE)
#' if (installed) {
#'   library(fgeo.analyze)
#'
#'   elev_list <- fgeo.x::elevation
#'   habitat <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
#'   census <- fgeo.analyze::luquillo_top3_sp
#'   species <- unique(census$sp)
#'   to_df(tt_test(census, species, habitat))
#' }
#'
#' # If working with elevation dataframe you must provide xdim and ydim
#' elev_df <- fgeo.x::elevation$col
#' hab2 <- fgeo_habitat(elev_df, gridsize = 20, n = 4, xdim = 320, ydim = 500)
#' str(hab2)
#' }
fgeo_habitat <- function(elevation, gridsize, n, ...) {
  out <- add_cluster(fgeo.tool::fgeo_topography(elevation, gridsize, ...), n)
  names(out) <- sub("cluster", "habitats", names(out))
  new_fgeo_habitat(out[c("gx", "gy", "habitats")])
}

new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}

