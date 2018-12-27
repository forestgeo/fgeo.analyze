#' A general function to calculate distances in a n-dimensional toroid.
#'
#' @description
#' By default (i.e. if the arguments `lower` and `upper` are not provided), this
#' function returns the distance in the Euclidean space by assuming borders
#' infinitely apart (i.e. points in a small portion of an infinitely large
#' toroid).
#'
#' The shortest distance in the toroid is the hypotenuse of the smallest
#' hyper-triangle. The 'internal' distance is the typical distance based on the
#' coordinates, as in the Euclidean space. The 'external' distance is crossing
#' borders, going around. There are only two ways of measuring distance along
#' each dimension.
#'
#' @param x A numeric matrix giving the coordinates (positions) of the points.
#' @param lower,upper Numeric vectors of length `ncol(x)`. The minimum and
#'   maximum possible values of the coordinates along each dimension.
#'
#' @author Gabriel Arellano
#'
#' @return A numeric matrix.
#' @export
#'
#' @examples
#' numeric_vec <- c(runif(10, min = 3, max = 5), runif(10, min = 13, max = 15))
#' x <- matrix(numeric_vec, ncol = 2)
#'
#' # Euclidean distances
#' d0 <- dist(x)
#' # default behaviour
#' d1 <- dist_in_torus(x)
#' # distances in the toroid
#' d2 <- dist_in_torus(x, lower = c(3, 13), upper = c(5, 15))
#'
#' par(mfrow = c(1, 3))
#' plot(x, xlim = c(3, 5), ylim = c(13, 15), xlab = "x", ylab = "y")
#' plot(c(d0), c(as.dist(d1)), main = "default = Euclidean = infinite toroid")
#' abline(0, 1)
#' plot(c(d0), c(as.dist(d2)), main = "finite toroid")
#' abline(0, 1)
#'
#' # `upper` and `lower` must be as long as `ncol(x)`
#' x <- matrix(runif(9), ncol = 3)
#' dist_in_torus(x, lower = c(0, 0, 0), upper = c(1, 1, 1))
dist_in_torus <- function(x,
                          lower = rep(-Inf, ncol(x)),
                          upper = rep(Inf, ncol(x))) {
  if (!is.matrix(x)) {
    warn(paste0("Coercing `x` to matrix.\n* `x` was of class ", class(x)))
    x <- as.matrix(x)
  }
  check_dist_in_torus(x = x, lower = lower, upper = upper)

  # Number of dimensions
  n <- ncol(x)
  # Size of the n-dimensional space considered
  ranges <- upper - lower

  # Internal and external cathetuses along each dimension:
  internal_cats <- sapply(
    1:n, function(i) abs(outer(x[, i], x[, i], "-")),
    simplify = "array"
  )
  external_cats <- sapply(1:n, function(i) ranges[i] - internal_cats[, , i])

  # The shortest cathetuses along each dimension define the smallest
  # hyper-triangle:
  shortest_cats <- pmin(internal_cats, external_cats)

  # Application of the Pythagorean theorem across layers:
  hypo <- sqrt(rowSums(shortest_cats^2, dims = 2))
  hypo
}

check_dist_in_torus <- function(x, lower, upper) {
  if (!is.numeric(x)) {
    msg <- paste0("`x` must be numeric.\n", "* It has type ", typeof(x))
    abort(msg)
  }

  stopifnot(length(lower) == ncol(x), length(upper) == ncol(x))
}
