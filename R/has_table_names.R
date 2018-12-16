#' Factory of predicates to check if a table has the same names as a reference.
#'
#' @param reference A dataframe.
#'
#' @return A closure.
#' @examples
#' stem <- data.frame(x = 1, y = 1)
#' tree <- data.frame(x = 1, z = 1)
#' has_table_names(stem)(stem)
#' has_table_names(stem)(tree)
#'
#' @family general predicates
#' @noRd
has_table_names <- function(reference) {
  function(.data) {
    has_expected_names <- all(utils::hasName(.data, names(reference)))
    if (has_expected_names) TRUE else FALSE
  }
}
