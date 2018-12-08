#' @importFrom fgeo.tool rename_matches
#' @importFrom fgeo.tool check_crucial_names is_duplicated is_multiple
#' @importFrom dplyr group_by ungroup filter select mutate summarize arrange
#' @importFrom dplyr count
#' @importFrom glue glue
#' @importFrom rlang set_names %||% abort warn inform
NULL

#' Functions used by ctfs
#'
#' These functions become necessary for functions that come from __ctfs__.
#'
#' @name ctfs
#' @keywords internal
#' @noMd
#' @importFrom grDevices dev.off graphics.off pdf
#' @importFrom graphics hist lines par plot points text
#' @importFrom stats dnorm median optim qbeta qt quantile rgamma rnorm runif sd
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Avoid CMD check warnings
utils::globalVariables(c(".data", "n"))
