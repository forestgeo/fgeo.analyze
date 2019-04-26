#' Coerce objects different fgeo classes "data.frame" or "tbl" (tibble).
#'
#' @param x An object of class tt_lst.
#' @param ... Arguments passed on to [base::as.data.frame()] (not used in
#'   [as_tibble()]).
#'
#' @seealso [base::data.frame()], [tibble::as_tibble()].
#' @return A [base::data.frame()] or [tibble::tibble()].
#'
#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

# Methods for class tt_lst --------------------------------------------------

#' @family habitat functions
#' @family methods for common generics
#' @export
as_tibble.tt_lst <- function(x, ...) {
  long_df <- tt_gather(x)
  out <- tt_create_df(tt_restructure(long_df))
  new_tt_df(out)
}

#' @family habitat functions
#' @family methods for common generics
#' @export
as.data.frame.tt_lst <- function(x, ...) {
  as.data.frame(unclass(as_tibble.tt_lst(x)), ...)
}

tt_gather <- function(x) {
  flip <- t(Reduce(rbind, x))
  as_tibble(gather_mat(flip, "metric", "sp", "value"))
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
    tibble::as_tibble(.name_repair = "minimal") %>%
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

#' @family demography functions
#' @family methods for common generics
#' @export
as.data.frame.demography_ctfs <- function(x, ...) {
  as.data.frame(unclass(as_tibble(x)), ...)
}
