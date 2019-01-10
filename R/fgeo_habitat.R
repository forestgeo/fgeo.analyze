#' Structure habitat data from measures of topography.
#'
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat (via
#' [fgeo_topography()])).
#' 2. It calculates habitats based on hierarchical clustering of the topographic
#' metrics from step 1 (via [stats::hclust()]).
#'
#' @section Input:
#' The main input can be either the elevation list that ForestGEO delivers, or
#' the element `col` of such list -- which is a dataframe containing the
#' elevation data. Notice that the required arguments vary according to the main
#' input (the elevation list or the elevation dataframe). Whatever the input,
#' the dataframe containing the elevation data must have columns `gx` and `gy`
#' or `x` and `y`.
#'
#' @author Rick Condit.
#'
#' @inheritParams fgeo_topography
#'
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
#' # Input a ForestGEO-like elevation list or dataframe
#' elevation_ls <- fgeo.x::elevation
#' habitats <- fgeo_habitat(
#'   elevation_ls,
#'   gridsize = 20, n = 4
#' )
#' # Or
#' elevation_df <- fgeo.x::elevation$col
#' habitats <- fgeo_habitat(
#'   elevation_df,
#'   gridsize = 20, n = 4,
#'   xdim = 320, ydim = 500
#' )
#'
#' str(habitats)
#' \dontrun{
#' fgeo_plot_is_installed <- requireNamespace("fgeo.plot", quietly = TRUE)
#' if (fgeo_plot_is_installed) {
#'   library(fgeo.plot)
#'
#'   autoplot(habitats)
#' }
#' }
#'
#' # Habitat data is useful for calculating species-habitat associations
#' census <- fgeo.x::tree6_3species
#' as_tibble(
#'   tt_test(census, habitats)
#' )
#' @family habitat functions
#' @family functions to construct fgeo classes
#' @export
fgeo_habitat <- function(elevation, gridsize, n, ...) {
  out <- add_cluster(fgeo_topography(elevation, gridsize, ...), n)
  names(out) <- sub("cluster", "habitats", names(out))
  new_fgeo_habitat(out[c("gx", "gy", "habitats")])
}

new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}
