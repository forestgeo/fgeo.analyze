#' Abundance and basal area, optionally by groups.
#'
#' @description
#' * [abundance()] counts the number of rows in a dataset, optionally by groups
#' created with [dplyr::group_by()] (similar to [dplyr::n()]). It warns if it
#' detects duplicated values of treeid.
#' * [basal_area()] sums the basal area of
#' all stems in a dataset, optionally by groups created with [group_by()]. It
#' warns if it detects duplicated values of stemid. It does not convert units
#' (but see examples).
#'
#' Both [abundance()] and [basal_area()] warn if they detect
#' multiple censusid and multiple plots.
#'
#' @details
#' You may want to calculate the abundance or basal area for a specific subset
#' of data (e.g. "alive" stems or stems which `dbh` is within some range).
#' Subsetting data is not the job of these functions. Instead see
#' [base::subset()], [dplyr::filter()], or `[`.
#'
#' @param data A dataframe. [basal_area()] requires a column named `dbh` (case
#'   insensitive).
#'
#' @seealso [dplyr::n()], [dplyr::group_by()].
#'
#' @examples
#' library(fgeo.tool)
#'
#' # abundance() -------------------------------------------------------------
#'
#' abundance(data.frame(1))
#'
#' # One stem per tree
#' tree <- tribble(
#'   ~TreeID, ~StemID, ~DBH,
#'   "1", "1.1", 11,
#'   "2", "2.1", 21
#' )
#'
#' abundance(tree)
#'
#' # One tree with multiple stems
#' stem <- tribble(
#'   ~TreeID, ~StemID, ~DBH,
#'   "1", "1.1", 11,
#'   "1", "1.2", 12
#' )
#'
#' abundance(stem)
#'
#' # Skip R CMD check for speed
#' \donttest{
#' # Similar but more realistic
#' assert_is_installed("fgeo.x")
#' stem <- fgeo.x::download_data("luquillo_stem5_random")
#'
#' abundance(stem)
#'
#' abundance(pick_main_stem(stem))
#' }
#'
#' vft <- tribble(
#'   ~PlotName, ~CensusID, ~TreeID, ~StemID, ~DBH,
#'   "p", 1, "1", "1.1", 10,
#'   "q", 2, "1", "1.1", 10
#' )
#'
#' # * Warns if it detects multiple values of censusid or plotname
#' # * Also warns if it detects duplicated values of treeid
#' abundance(vft)
#'
#' # If trees have buttressess, the data may have multiple stems per treeid or
#' # multiple measures per stemid.
#' vft2 <- tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'   1, "1", "1.1", 88, 130,
#'   1, "1", "1.1", 10, 160,
#'   1, "2", "2.1", 20, 130,
#'   1, "2", "2.2", 30, 130,
#' )
#'
#' # You should count only the main stem of each tree
#' (main_stem <- pick_main_stem(vft2))
#'
#' abundance(main_stem)
#'
#' vft3 <- tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'   1, "1", "1.1", 20, 130,
#'   1, "1", "1.2", 10, 160, # Main stem
#'   2, "1", "1.1", 12, 130,
#'   2, "1", "1.2", 22, 130 # Main stem
#' )
#'
#' # You can compute by groups
#' by_census <- group_by(vft3, CensusID)
#' (main_stems_by_census <- pick_main_stem(by_census))
#'
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
#'
#' basal_area(main_stemids)
#'
#' # You can compute by groups
#' basal_area(by_census)
#'
#' # Skip R CMD check for speed
#' \donttest{
#' measurements_is_installed <- requireNamespace("measurements", quietly = TRUE)
#' if (measurements_is_installed) {
#'   library(measurements)
#'
#'   # Convert units
#'   ba <- basal_area(by_census)
#'   ba$basal_area_he <- conv_unit(
#'     ba$basal_area,
#'     from = "mm2",
#'     to = "hectare"
#'   )
#'
#'   ba
#' }
#' }
#'
#' @family functions for abundance and basal area
#' @name abundance
NULL
with_anycase_group_df <- function(.summary, side_effects) {
  function(data) {
    # Census and ViewFull tables have column names with different case. To
    # handle both kinds of dataset we lowercase column and group names.
    low_nms <- groups_lower(set_names(data, tolower))
    # Allow multiple, different side effects for different summaries
    lapply(side_effects, function(.f) .f(low_nms))

    result <- .summary(low_nms)

    # Restore the original case of names and relevant groups
    restore_input_names_output_groups(result, data)
  }
}

#' @rdname abundance
#' @export
abundance <- with_anycase_group_df(
  abundance_df,
  list(warn_if_needed_treeid, warn_if_needed_plotname_censusid)
)

#' @rdname abundance
#' @export
basal_area <- with_anycase_group_df(
  basal_area_df,
  list(warn_if_needed_stemid, warn_if_needed_plotname_censusid)
)

abundance_df <- function(data) {
  g <- dplyr::group_vars(data)
  out <- summarize(data, n = n())
  dplyr::grouped_df(out, g)
}

basal_area_df <- function(data) {
  g <- dplyr::group_vars(data)
  if (rlang::is_empty(data)) {
    data <- tibble(dbh = double(0))
  }
  out <- summarize(
    data,
    basal_area = sum(basal_area_dbl(.data$dbh), na.rm = TRUE)
  )
  dplyr::grouped_df(out, g)
}

basal_area_dbl <- function(x) {
  1 / 4 * pi * (x)^2
}

groups_lower <- function(data) {
  dplyr::grouped_df(data, tolower(dplyr::group_vars(data)))
}

#' Get the correct grouping variables.
#'
#' This funciton is useful when working inside a function that works with
#' lowercase variable and group names. The output of such function needs to may
#' have the correct grouping variables but with the wrong case. This function
#' outputs a sting of the grouping variable in `x` with the case of `y`.
#'
#' @param data A dataframe which groups are ok but lowercase.
#' @param data_ref A reference dataframe which gropus are not ok but have
#'   correct case.
#'
#' @examples
#' out <- dplyr::grouped_df(tibble::tibble(x = 1, y = 1, z = 1), c("x", "y"))
#' out
#' ref <- dplyr::grouped_df(rlang::set_names(out, toupper), c("X"))
#' group_vars_restore(out, ref)
#' @noRd
group_vars_restore <- function(data, data_ref) {
  in_ref <- fgeo.tool::detect_insensitive(
    dplyr::group_vars(data),
    dplyr::group_vars(data_ref)
  )

  fgeo.tool::extract_insensitive(
    dplyr::group_vars(data),
    dplyr::group_vars(data_ref)
  )
}

restore_input_names_output_groups <- function(data, data_ref) {
  data_ <- rename_matches(data, data_ref)
  g <- group_vars_restore(data_, data_ref)
  dplyr::grouped_df(ungroup(data_), g)
}

# Warn only if data contains specific `name`s.
warn_if_needed_plotname_censusid <- function(data) {
  warn_if_has_var(
    data,
    name = "censusid", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to group by censusid?"
  )
  warn_if_has_var(
    data,
    name = "plotname", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to pick a single plot?"
  )

  invisible(data)
}

warn_if_needed_treeid <- function(data) {
  warn_if_has_var(
    data,
    name = "treeid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick main stems?"
  )
  invisible(data)
}

warn_if_needed_stemid <- function(data) {
  warn_if_has_var(
    data,
    name = "stemid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick largest `hom` values?"
  )
  invisible(data)
}

warn_if_has_var <- function(data, name, predicate, problem, hint) {
  if (rlang::has_name(data, name)) {
    msg <- glue("`{name}`: {problem} values were detected. {hint}")
    fgeo.tool::flag_if_group(data, name, predicate, warn, msg)
  }
}
