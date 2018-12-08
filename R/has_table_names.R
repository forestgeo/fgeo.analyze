#' Factory of predicates to check if a table has the same names as a reference.
#'
#' @param reference A dataframe.
#' @family general predicates
#'
#' @return A closure.
#' @keywords internal
#' @noRd
#' @examples
#' stem <- data.frame(x = 1, y = 1)
#' tree <- data.frame(x = 1, z = 1)
#' has_table_names(stem)(stem)
#' has_table_names(stem)(tree)
#' @name has_table_names
has_table_names <- function(reference) {
  function(.data) {
    has_expected_names <- all(utils::hasName(.data, names(reference)))
    if (has_expected_names) TRUE else FALSE
  }
}
