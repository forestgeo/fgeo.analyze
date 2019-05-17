#' @importFrom dplyr group_by ungroup filter select mutate summarize arrange
#' @importFrom dplyr count n
#' @importFrom fgeo.tool check_crucial_names is_duplicated is_multiple
#' @importFrom fgeo.tool rename_matches
#' @importFrom glue glue glue_collapse
#' @importFrom graphics hist lines par plot points text
#' @importFrom MASS boxcox
#' @importFrom rlang abort inform warn expr eval_tidy expr_label %||% set_names
#' @importFrom stats dnorm median optim qbeta qt quantile rgamma rnorm runif sd
#' @importFrom stats nls var nls.control predict resid
#' @importFrom tibble tibble as_tibble
NULL

#' @importFrom fgeo.tool assert_is_installed
#' @export
#' @keywords internal
fgeo.tool::assert_is_installed

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
utils::globalVariables(c(".data", "."))
