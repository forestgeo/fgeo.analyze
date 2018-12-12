#' Create objects of class "data.frame" from other fgeo classes.
#'
#' Most of the popular, general-purpose tools for data science input objects of
#' class "data.frame" (<https://www.tidyverse.org/>). However, several __fgeo__
#' functions (either inherited from the original CTFS R Package or contributed
#' by ForestGEO partners) output data of different class. Taking as input
#' different classes of __fgeo__ objects, `to_df()` provides a simple,
#' consistent way to create dataframes.
#'
#' This generic provides methods for classes that cannot be correctly coerced
#' simply with [base::as.data.frame()] (or similar functions from the
#' __tibble__ package).
#'
#' @param .x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [to_df.krig_lst()].
#'
#' @family fgeo generics
#' @keywords internal
#'
#' @return A dataframe.
#' @export
to_df <- function(.x, ...) {
  UseMethod("to_df")
}

#' @export
to_df.default <- function(.x, ...) {
  abort(glue("Can't deal with data of class {class(.x)}"))
}

# Class tt_lst ------------------------------------------------------------

#' Dataframe objects of class "tt_lst".
#'
#' This method creates a dataframe from the output of `fgeo.analyze::tt_test()`
#' (which is a list of class "tt_lst").
#'
#' @param .x An object of class tt_lst.
#' @param ... Other arguments passed to [to_df()].
#'
#' @seealso [to_df()].
#'
#' @family methods for fgeo generics
#' @family habitat functions
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("fgeo.analyze")) {
#'   library(fgeo.analyze)
#'
#'   cns <- fgeo.analyze::luquillo_top3_sp
#'   hab <- fgeo.x::habitat
#'
#'   tt_lst <- tt_test(cns, unique(cns$sp), hab)
#'   to_df(tt_lst)
#' }
#' }
to_df.tt_lst <- function(.x, ...) {
  long_df <- tt_gather(.x)
  out <- tt_create_df(tt_restructure(long_df))
  new_tt_df(out)
}

tt_gather <- function(.x) {
  flip <- t(Reduce(rbind, .x))
  tibble::as.tibble(gather_mat(flip, "metric", "sp", "value"))
}

tt_restructure <- function(.x) {
  with_habitat <- spread_metric_value(separate_habitat_metric(.x))
  metrics <- purrr::map(with_habitat, ~.x["metric", ])[1][[1]]
  list(with_habitat = with_habitat, metrics = metrics)
}

separate_habitat_metric <- function(x) {
  dplyr::mutate(x,
    habitat = gsub("^.*\\.([0-9]+$)", "\\1", .data$metric),
    metric = gsub("(^.*)\\.[0-9]+$", "\\1", .data$metric)
  )
}

separate_habitat_sp <- function(x) {
  dplyr::mutate(x,
    habitat = gsub(".*[.]([0-9]+$)", "\\1", .data$species_habitat),
    sp = gsub("(.*)[.][0-9]+$", "\\1", .data$species_habitat),
    species_habitat = NULL
  )
}

spread_metric_value <- function(with_habitat){
  with_habitat %>%
    split(interaction(with_habitat$sp, with_habitat$habitat)) %>%
    purrr::map(~.x[c("metric", "value")]) %>%
    purrr::map(t)
}

tt_create_df <- function(tt_data) {
  out <- tt_data$with_habitat %>%
    purrr::map(~.x["value", ]) %>%
    purrr::imap(~c(.y, .x)) %>%
    purrr::reduce(rbind) %>%
    tibble::as_tibble() %>%
    rlang::set_names(c("species_habitat", tt_data$metrics)) %>%
    purrr::modify_at(.at = tt_data$metrics, as.numeric) %>%
    dplyr::arrange(.data$species_habitat)

  separate_habitat_sp(out)[c("habitat", "sp", tt_data$metrics)]
}

#' Form the authors of tt_test().
#' The Rep.Agg.Neut columns for each habitat indicate whether the sp is
#' significantly repelled (-1), aggregated (1), or neutraly distributed (0) on
#' the habitat in question.
#' @keywords internal
#' @noRd
explain_distribution <- function(x) {
  dplyr::mutate(x,
    distribution = dplyr::case_when(
      .data$distribution == 1 ~ "aggregated",
      .data$distribution == -1 ~ "repelled",
      .data$distribution == 0 ~ "neutral",
      TRUE ~ NA_character_
    )
  )
}

reorganize_columns <- function(x) {
  first <- c("habitat", "sp", "distribution", "stem_count")
  x[c(first, setdiff(names(x), first))]
}

new_tt_df <- function(.x) {
  stopifnot(is.data.frame(.x))
  structure(.x, class = c("tt_df", class(.x)))
}

# Class demography_ctfs ---------------------------------------------------

#' Dataframe objects of class "demography_impl".
#'
#' @param .x An object of class demography_impl.
#' @param ... Other arguments passed to `to_df()`.
#'
#' @seealso [to_df()].
#'
#' @family methods for fgeo generics
#' @family demography functions
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

