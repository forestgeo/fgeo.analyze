#' Check if an object contains specific names.
#'
#' @param x A named object.
#' @param nms String; names expected to be found in `x`.
#'
#' @family functions to check inputs.
#' @family functions for developers
#' @family general functions to assert
#'
#'
#' @return Invisible `x`, or an error with informative message.
#' 
#' @family exported functions for internal use
#' @export
#' @keywords internal
#'
#' @examples
#' v <- c(x = 1)
#' check_crucial_names(v, "x")
#'
#' dfm <- data.frame(x = 1)
#' check_crucial_names(dfm, "x")
check_crucial_names <- function(x, nms) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(nms))

  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
  }

  stop(
    "Ensure your data set has these variables:\n", glue_comma(nms),
      call. = FALSE
  )
}
