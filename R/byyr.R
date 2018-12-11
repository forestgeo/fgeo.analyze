#' Create tables of abundance and basal area by year.
#'
#' * `abundance_byyr()` first picks the main stem of each tree (see
#' ?[fgeo.tool::pick_main_stem()] and then, for each species and each
#' (round mean) year of measurement, counts the number of
#' trees. The result includes __main stems__ within a given dbh range.
#' * `basal_area_byyr()` first sums the basal basal area of all stems of each
#' tree, and then, for each species and each (round mean) year of measurement,
#' sums the basal area of all trees. The result includes all stems within a
#' given dbh range (notice the difference with `abundance_byyr()`).
#'
#' You don't need to pick stems by status before feeding data to these
#' functions. Doing so may make your code more readable but it should not affect
#' the result. This is because the expressions passed to `...` pick data by
#' `dbh` and exclude missing the `dbh` values associated to non-alive stems,
#' including dead, missing, and gone.
#'
#' @param vft A ForestGEO-like dataframe; particularly a ViewFullTable. As such,
#'   it should contain columns `PlotName`, `CensusID`, `TreeID`, `StemID`,
#'   `Status`, `DBH`, `Genus`, `SpeciesName`, `ExactDate`, `PlotCensusNumber`,
#'   `Family`, `Tag`, and `HOM`. `ExactDate` should contain dates from
#'   1980-01-01 to the present day in the format yyyy-mm-dd.
#' @param ... Expressions to pick main stems of a specific `dbh` range.
#'
#' @family functions for abundance and basal area
#'
#' @seealso [fgeo.tool::pick_main_stem()].
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' library(fgeo.tool)
#'
#' vft <- example_byyr
#' vft
#' abundance_byyr(vft, DBH >= 10, DBH < 20)
#' abundance_byyr(vft, DBH >= 10)
#' basal <- basal_area_byyr(vft, DBH >= 10)
#' basal
#'
#' \dontrun{
#' # Convert units
#' # This exmaple requires the following packages (see `?install.packages`):
#' required_packages <- c("measurements", "purrr")
#' not_installed <- !all(sapply(required_packages, requireNamespace, quietly = T))
#' if (not_installed) {
#'   stop("To run this section please install the required packages.")
#' } else {
#'   years <- c("yr_2001", "yr_2002")
#'   basal_he <- purrr::modify_at(
#'     basal,
#'     .at = years,
#'     .f =  ~measurements::conv_unit(.x, from = "mm2", to = "hectare")
#'   )
#'   basal_he
#'
#'   # Standardize
#'   number_of_hectares <- 50
#'   purrr::map_at(
#'     basal_he,
#'     .at = years,
#'     .f = ~.x / number_of_hectares
#'   )
#' }
#' }
abundance_byyr <- function(vft, ...) {
  low_nms  <- check_byyr(set_names(vft, tolower))
  crucial <- c("plotname", "tag")
  low_nms  <- check_crucial_names(low_nms, crucial)

  main_stems <- fgeo.tool::pick_main_stem(low_nms)

  with_years <- add_years(pick_byyr(main_stems, ...))
  out <- with_years %>%
    group_by(.data$plotname, .data$year, .data$family, .data$species) %>%
    dplyr::summarize(n = dplyr::n_distinct(.data$treeid)) %>%
    ungroup() %>%
    select(-.data$plotname) %>%
    select(.data$species, .data$family, dplyr::everything()) %>%
    tidyr::spread(.data$year, n, fill = 0) %>%
    arrange(.data$species, .data$family)

  tidy_byyr_names(rename_matches(out, vft))
}

#' @rdname abundance_byyr
#' @export
basal_area_byyr <- function(vft, ...) {
  low_nms <- check_byyr(set_names(vft, tolower))

  main_stemids <- fgeo.tool::pick_main_stemid(low_nms)
  with_years <- add_years(pick_byyr(main_stemids, ...))
  out <- with_years %>%
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area() %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0)

  tidy_byyr_names(rename_matches(out, vft))
}

# Checks ------------------------------------------------------------------

check_byyr <- function(vft) {
  stopifnot(is.data.frame(vft))
  crucial <- c(
    "genus", "speciesname", "family", "status", "dbh", "exactdate",
    "plotcensusnumber"
  )
  check_crucial_names(vft, crucial)

  dates <- unique(vft$exactdate)
  if (all(is.na(lubridate::ymd(dates)))) {
    abort(
      "Can't parse `exactdates`. Try parsing dates with `lubridate::ymd()`."
    )
  }

  too_early <- lubridate::ymd(dates) < lubridate::ymd("1980-01-01")
  too_late <- lubridate::ymd(dates) > lubridate::today()
  if (any(too_early || too_late)) {
    warn("Dates should be from 1980-present and have format yyy-mm-dd.")
  }

  invisible(vft)
}

# Helpers -----------------------------------------------------------------

pick_byyr <- function(vft, ...) {
  dots <- lowercase_var(..., .var = "dbh")
  flag_if_not_expression_of_var(dots, .flag = rlang::abort, .var = "dbh")
  dplyr::filter(vft, !!! dots)
}

add_years <- function(x) {
  drop_if_missing_dates(x) %>%
    mean_years() %>%
    fgeo.tool::drop_if_na("year")
}

mean_years <- function(vft) {
  years <- vft %>%
    set_names(tolower) %>%
    group_by(.data$plotcensusnumber) %>%
    summarize(
      year = round(mean(lubridate::year(.data$exactdate), na.rm = TRUE))
    ) %>%
    unique() %>%
    arrange(.data$plotcensusnumber) %>%
    ungroup() %>%
    rename_matches(vft)

  dplyr::left_join(vft, years, by = "plotcensusnumber") %>%
    mutate(species = paste(.data$genus, .data$speciesname)) %>%
    arrange(.data$year)
}

drop_if_missing_dates <- function(x) {
  missing_dates <- is.na(insensitive(x)$exactdate)
  if (any(missing_dates)) {
    warn("Detected and ignoring missing dates.")
  }
  x <- x[!missing_dates, , drop = FALSE]
  invisible(x)
}

tidy_byyr_names <- function(x) {
  x <- rlang::set_names(x, tolower)
  spp_family <- c("species", "family")
  yr_nms <- setdiff(names(x), spp_family)
  names(x) <- c(spp_family, glue("yr_{yr_nms}"))
  x
}

#' Inform, warn or abort if not all expresisons refer to a given variable.
#'
#' @param dots Expressions, usually passed to dplyr::filter() via `...`.
#' @param .flag Rlang flag funcitons: inform, warn, and abort.
#' @param .var String of lenght one giving the name of the variable expected to
#'   be referred in the expressions passed to `...`.
#' @keywords internal
#' @noRd
flag_if_not_expression_of_var <- function(dots, .flag, .var) {
  .dots <- rlang::expr_deparse(dots)
  if (!any(grepl(.var, .dots))) {
    flag_is_abort <- identical(.flag, rlang::abort)
    request <- ifelse(flag_is_abort, "must", "should")
    msg <- glue("All expressions passed to `...` {request} refer to `{.var}`.")
    .flag(msg)
  }

  invisible(dots)
}

#' For each expressions in `...`, lowercase the name of a given variable.
#' @keywords internal
#' @noRd
lowercase_var <- function(..., .var) {
  lowercase_each <- function(dots, .var) {
    dots <- gsub(.var, .var, rlang::expr_deparse(dots), ignore.case = TRUE)
    rlang::parse_expr(dots)
  }

  lapply(rlang::exprs(...), lowercase_each, .var)
}

