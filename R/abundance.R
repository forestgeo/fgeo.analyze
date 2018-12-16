#' Abundance and basal area, optionally by groups.
#'
#' `abundance()` counts woods by counting the number of rows in a dataset,
#' optionally by groups created with [group_by()] (similar to [dplyr::n()]). It
#' warns if it detects duplicated values of treeid. `basal_area()` sums the
#' basal area of all woods in a dataset, optionally by groups created with
#' [group_by()]. It warns if it detects duplicated values of stemid. It does not
#' convert units (but see examples). Both `abundance()` and
#' `basal_area()` warn if they detect multiple censusid and multiple plots.
#'
#' You may want to calculate the abundance or basal area for a specific subset
#' of data (e.g. "alive" stems or stems which `dbh` is within some range).
#' Subsetting data is not the job of these functions. Instead see [subset()],
#' [dplyr::filter()], or `[`.
#'
#' @param x A dataframe. `basal_area()` requires a column named `dbh` (case
#'   insensitive).
#'
#' @seealso [dplyr::n()], [group_by()].
#'
#' @examples
#' library(dplyr)
#' library(fgeo.tool)
#'
#' # abundance() -------------------------------------------------------------
#'
#' # Similar to dplyr::n()
#' abundance(data.frame(1))
#'
#' vft <- tibble::tribble(
#'   ~PlotName, ~CensusID, ~TreeID, ~StemID, ~DBH,
#'         "p",         1,     "1",   "1.1",   10,
#'         "q",         2,     "1",   "1.1",   10
#' )
#'
#' # * Warns if it detects multiple values of censusid or plotname
#' # * Also warns if it detects duplicated values of treeid
#' abundance(vft)
#'
#' # You should probably work with a single plotname.
#' # Yet your data may have multiple stems per treeid and even multiple measures
#' # per stemid (when trees have buttressess).
#' vft2 <- tibble::tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'           1,     "1",   "1.1",   88,  130,
#'           1,     "1",   "1.1",   10,  160,
#'           1,     "2",   "2.1",   20,  130,
#'           1,     "2",   "2.2",   30,  130,
#' )
#'
#' # You should count only the main stem of each tree
#' (main_stem <- pick_main_stem(vft2))
#' abundance(main_stem)
#'
#' vft3 <- tibble::tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'           1,     "1",   "1.1",   20,  130,
#'           1,     "1",   "1.2",   10,  160,  # Main stem
#'           2,     "1",   "1.1",   12,  130,
#'           2,     "1",   "1.2",   22,  130   # Main stem
#' )
#'
#' # You can compute by groups
#' by_census <- group_by(vft3, CensusID)
#' (main_stems_by_census <- pick_main_stem(by_census))
#' abundance(main_stems_by_census)
#'
#' # basal_area() ------------------------------------------------------------
#'
#' # Data must have a column named dbh (case insensitive)
#' basal_area(data.frame(dbh = 1))
#'
#' # * Warns if it detects multiple values of censusid or plotname
#' # * Also warns if it detects duplicated values of stemid
#' basal_area(vft)
#'
#' # First you may pick the main stemid of each stem
#' (main_stemids <- pick_main_stemid(vft2))
#' basal_area(main_stemids)
#'
#' # You can compute by groups
#' basal_area(by_census)
#'
#' \dontrun{
#' # Convert units
#' missing_measurements <- !requireNamespace("measurements", quietly = TRUE)
#' if (missing_measurements) stop("Please run `install.packages('measurements')`")
#' ba <- basal_area(by_census)
#' ba$basal_area_he <- measurements::conv_unit(
#'   ba$basal_area,
#'   from = "mm2",
#'   to = "hectare"
#' )
#' ba
#' }
#' @family functions for abundance and basal area
#' @name abundance
NULL
with_anycase_group_df <- function(.summary, side_effects) {
  function(x) {
    # Census and ViewFull tables have column names with different case. To
    # handle both kinds of dataset we lowercase column and group names.
    low_nms <- groups_lower(set_names(x, tolower))
    # Allow multiple, different side effects for different summaries
    lapply(side_effects, function(.f) .f(low_nms))

    result <- .summary(low_nms)

    # Restore the original case of names and relevant groups
    restore_input_names_output_groups(result, x)
  }
}

#' @export
#' @rdname abundance
abundance <- with_anycase_group_df(
  abundance_df, list(warn_if_needed_treeid, warn_if_needed_plotname_censusid)
)

#' @export
#' @rdname abundance
basal_area <- with_anycase_group_df(
  basal_area_df, list(warn_if_needed_stemid, warn_if_needed_plotname_censusid)
)

abundance_df <- function(x) {
  g <- dplyr::group_vars(x)
  out <- summarize(x, n = n())
  dplyr::grouped_df(out, g)
}

basal_area_df <- function(x) {
  g <- dplyr::group_vars(x)
  if (rlang::is_empty(x)) {
    x <- tibble(dbh = double(0))
  }
  out <- summarize(x, basal_area = sum(basal_area_dbl(.data$dbh), na.rm = TRUE))
  dplyr::grouped_df(out, g)
}

basal_area_dbl <- function(x) {
  1 / 4 * pi * (x)^2
}

groups_lower <- function(x) {
  dplyr::grouped_df(x, tolower(dplyr::group_vars(x)))
}

#' Get the correct grouping variables.
#'
#' This funciton is useful when working inside a function that works with
#' lowercase variable and group names. The output of such function needs to may
#' have the correct grouping variables but with the wrong case. This function
#' outputs a sting of the grouping variable in `x` with the case of `y`.
#'
#' @param x A dataframe which groups are ok but lowercase.
#' @param y A reference dataframe which gropus are not ok but have correct case.
#'
#' @examples
#' out <- dplyr::grouped_df(tibble::tibble(x = 1, y = 1, z = 1), c("x", "y"))
#' out
#' ref <- dplyr::grouped_df(rlang::set_names(out, toupper), c("X"))
#' group_vars_restore(out, ref)
#'
#' @noRd
group_vars_restore <- function(x, y) {
  in_ref <- fgeo.tool::detect_insensitive(
    dplyr::group_vars(x),
    dplyr::group_vars(y)
  )

  fgeo.tool::extract_insensitive(
    dplyr::group_vars(x),
    dplyr::group_vars(y)
  )
}

restore_input_names_output_groups <- function(out, .data) {
  out <- rename_matches(out, .data)
  g <- group_vars_restore(out, .data)
  dplyr::grouped_df(ungroup(out), g)
}

# Only if data contains specific `name`s.
warn_if_needed_plotname_censusid <- function(.x) {
  warn_if_has_var(
    .x, name = "censusid", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to group by censusid?"
  )
  warn_if_has_var(
    .x, name = "plotname", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to pick a single plot?"
  )

  invisible(.x)
}

warn_if_needed_treeid <- function(.x) {
  warn_if_has_var(
    .x, name = "treeid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick main stems?"
  )
  invisible(.x)
}

warn_if_needed_stemid <- function(.x) {
  warn_if_has_var(
    .x, name = "stemid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick largest `hom` values?"
  )
  invisible(.x)
}

warn_if_has_var <- function(.x, name, predicate, problem, hint) {
  if (utils::hasName(.x, name)) {
    msg <- glue("`{name}`: {problem} values were detected. {hint}")
    fgeo.tool::flag_if_group(.x, name, predicate, warn, msg)
  }
}
