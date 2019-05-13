#' Create a vector or dataframe with case insensitive name matching.
#'
#' @param x vector or dataframe to modify.
#' @seealso `httr::insensitive()`.
#'
#' @section Acknowledgment:
#' Thanks to Neil Richardson for recommending this function
#' (https://twitter.com/enpiar), and to Hadley Wickham for developing
#' `?httr::insensitive()`, which is the starting point of this function.
#'
#' @examples
#' x <- c("abc" = 1, "def" = 2)
#' x["ABC"]
#' y <- insensitive(x)
#' y["ABC"]
#' y[["ABC"]]
#' y$ABC
#'
#' vft <- data.frame(TreeID = 1)
#' insensitive(vft)[["TreeID"]]
#' insensitive(vft)[["treeid"]]
#' insensitive(vft)["TreeID"]
#' insensitive(vft)["treeid"]
#' insensitive(vft)$TreeID
#' insensitive(vft)$treeid
#'
#' # Works
#' transform(insensitive(vft), new = treeid)
#' # dplyr::mutate(insensitive(vft), new = treeid)
#' # Fails
#' # transform(insensitive(vft), new = TreeID)
#' # dplyr::mutate(insensitive(vft), new = TreeID)
#' @family functions for developers
#' @family general functions to deal with names
#' @noRd
insensitive <- function(x) {
  if (!rlang::is_named(x)) warning("`x` should be named.", call. = FALSE)
  names(x) <- tolower(names(x))
  structure(x, class = c("insensitive", class(x)))
}

#' @export
`[.insensitive` <- function(x, i, ...) {
  if (is.character(i)) {
    i <- tolower(i)
  }

  NextMethod()
}

#' @export
`[[.insensitive` <- `[.insensitive`

#' @export
"$.insensitive" <- function(x, name) {
  name <- tolower(name)
  x[[name]]
}
