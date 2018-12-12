#' @export
to_df.default <- function(.x, ...) {
  rlang::abort(glue("Can't deal with data of class {class(.x)}"))
}

# Class demography_impl ---------------------------------------------------

#' Dataframe objects of class "demography_impl".
#'
#' @param .x An object of class demography_impl.
#' @param ... Other arguments passed to `to_df()`.
#'
#' @seealso [to_df()].
#'
#' @family methods for fgeo generics
#'
#' @return A (tibble) dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' if (!requireNamespace("fgeo.ctfs")) {
#' library(fgeo.ctfs)
#'
#' census1 <- fgeo.x::tree5
#' census2 <- fgeo.x::tree6
#'
#' by <- interaction(census1$sp, census1$quadrat, sep = "__")
#' .x <- recruitment_impl(census1, census2, split1 = by)
#' head(to_df(.x))
#' }
#' }
to_df.demography_ctfs <- function(.x, ...) {
  malformed <- !is.null(attr(.x, "split2"))
  if (malformed) {
    abort(glue("
      Can't deal with data created with `split2` (deprecated).
      * Bad: `split1 = x1, split2 = x2`
      * Good: `split1 = interaction(x1, x2)`
    "))
  }

  result <- as.data.frame(Reduce(cbind, .x), stringsAsFactors = FALSE)
  result <- stats::setNames(result, names(.x))

  has_groups <- nrow(result) > 1
  if (has_groups) {
    result$groups <- rownames(result)
    result <- result[c("groups", setdiff(names(result), "groups"))]
  }
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

