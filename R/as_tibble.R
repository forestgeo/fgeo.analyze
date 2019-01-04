#' Coerce objects of different fgeo classes to "data.frame" and "tbl" (tibble).
#'
#' @param x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @return
#'     * `as.data.drame` returns an object of class "data.frame"
#'     * `as_tibble` returns an object of class "tibble".
#'
#' @seealso [base::as.data.frame()], [as_tibble()].
#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

# Methods for class tt_lst --------------------------------------------------

#' Coerce objects of class "tt_lst" to "data.frame" and "tbl" (tibble).
#'
#' This method creates a dataframe from the output of `tt_test()`
#' (which is a list of class "tt_lst").
#'
#' @param x An object of class tt_lst.
#' @param ... Not used.
#'
#' @seealso [as_tibble()].
#' @return A dataframe.
#'
#' @examples
#' census <- fgeo.x::tree6_3species
#' habitat <- fgeo.x::habitat
#'
#' as_tibble(
#'   tt_test(census, habitat)
#' )
#' @family habitat functions
#' @family methods for common generics
#' @export
as_tibble.tt_lst <- function(x, ...) {
  long_df <- tt_gather(x)
  out <- tt_create_df(tt_restructure(long_df))
  new_tt_df(out)
}

#' @export
#' @rdname as_tibble.tt_lst
as.data.frame.tt_lst <- function(x, ...) {
  as.data.frame(
    unclass(as_tibble.tt_lst(x))
  )
}

tt_gather <- function(x) {
  flip <- t(Reduce(rbind, x))
  as.tibble(gather_mat(flip, "metric", "sp", "value"))
}

tt_restructure <- function(x) {
  with_habitat <- spread_metric_value(separate_habitat_metric(x))
  metrics <- purrr::map(with_habitat, ~ .x["metric", ])[1][[1]]
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

spread_metric_value <- function(with_habitat) {
  with_habitat %>%
    split(interaction(with_habitat$sp, with_habitat$habitat)) %>%
    purrr::map(~ .x[c("metric", "value")]) %>%
    purrr::map(t)
}

tt_create_df <- function(tt_data) {
  out <- tt_data$with_habitat %>%
    purrr::map(~ .x["value", ]) %>%
    purrr::imap(~ c(.y, .x)) %>%
    purrr::reduce(rbind) %>%
    tibble::as_tibble() %>%
    rlang::set_names(c("species_habitat", tt_data$metrics)) %>%
    purrr::modify_at(.at = tt_data$metrics, as.numeric) %>%
    dplyr::arrange(.data$species_habitat)

  separate_habitat_sp(out)[c("habitat", "sp", tt_data$metrics)]
}

reorganize_columns <- function(x) {
  first <- c("habitat", "sp", "association", "stem_count")
  x[c(first, setdiff(names(x), first))]
}

new_tt_df <- function(x) {
  stopifnot(is.data.frame(x))
  structure(x, class = c("tt_df", class(x)))
}

# Methods for class demography_ctfs ----------------------------------------

#' Coerce objects of class "demography_ctfs" to "data.frame" and "tbl" (tibble).
#'
#' @param x An object of class demography_ctfs.
#' @param ... Not used.
#'
#' @seealso [as_tibble()].
#' @return A (tibble) dataframe.
#'
#' @examples
#' census1 <- fgeo.x::tree5
#' census2 <- fgeo.x::tree6
#' by_sp_and_quadrat <- interaction(census1$sp, census1$quadrat)
#'
#' demography_result <- recruitment_ctfs(
#'   census1,
#'   census2,
#'   split1 = by_sp_and_quadrat
#' )
#'
#' as_tibble(demography_result)
#' @family demography functions
#' @family methods for common generics
#' @export
as_tibble.demography_ctfs <- function(x, ...) {
  malformed <- !is.null(attr(x, "split2"))
  if (malformed) {
    abort(glue("
      Can't deal with data created with `split2` (deprecated).
      * Bad: `split1 = x1, split2 = x2`
      * Good: `split1 = interaction(x1, x2)`
    "))
  }

  result <- as.data.frame(Reduce(cbind, x), stringsAsFactors = FALSE)
  result <- stats::setNames(result, names(x))

  has_groups <- nrow(result) > 1
  if (has_groups) {
    result$groups <- rownames(result)
    result <- result[c("groups", setdiff(names(result), "groups"))]
  }
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

#' @export
#' @rdname as_tibble.demography_ctfs
as.data.frame.demography_ctfs <- function(x, ...) {
  as.data.frame(
    unclass(as_tibble(x))
  )
}
