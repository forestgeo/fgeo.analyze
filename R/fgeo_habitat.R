#' Create habitat data from measures of topography.
#'
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat.
#' 2. It calculates habitats based on hierarchical clustering of the topographic
#' metrics from step 1.
#'
#' @author Richard Condit.
#'
#' @inheritParams fgeo_topography
#' @param n Integer. Number of cluster-groups to construct (passed to the
#'   argument `k` to [stats::cutree()]).
#' @param ... Arguments passed to [fgeo_topography()].
#'
#' @seealso `fgeo.plot::autoplot.fgeo_habitat()`, [fgeo_topography()].
#'
#' @return A dataframe of subclass fgeo_habitat, with columns `gx` and `gy`,
#'   rounded with accuracy determined by `gridsize`, and column `habitats`, with
#'   as many distinct integer values as determined by the argument `n`.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' # Input a ForestGEO-like elevation list or dataframe
#' elevation_ls <- fgeo.x::elevation
#' habitats <- fgeo_habitat(
#'   elevation_ls,
#'   gridsize = 20, n = 4
#' )
#'
#' str(habitats)
#'
#' # Habitat data is useful for calculating species-habitat associations
#' census <- fgeo.x::tree6_3species
#' as_tibble(
#'   tt_test(census, habitats)
#' )
#' @family habitat functions
#' @family functions to construct fgeo classes
#' @export
fgeo_habitat <- function(elev, gridsize, n, ...) {
  out <- add_cluster(fgeo_topography(elev, gridsize, ...), n)
  names(out) <- sub("cluster", "habitats", names(out))
  new_fgeo_habitat(out[c("gx", "gy", "habitats")])
}

new_fgeo_habitat <- function(data) {
  structure(data, class = c("fgeo_habitat", class(data)))
}
