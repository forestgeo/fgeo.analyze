#' Recruitment, mortality, and growth.
#'
#' These functions are inherited from the the CTFS-R package. Compared to the
#' original functions, these ones have a similar interface but use more
#' conservative defaults and allow suppressing messages. These functions also
#' feature formal tests, bug fixes, additional assertions, and improved
#' messages. (These functions are the internal implementation of their
#' in-development analogs -- hence the suffix `_impl`.)
#'
#' Survivors are all individuals alive in both censuses, with `status == A` in
#' the first census, and larger than the minimum dbh in the first census. The
#' total population in the second census includes all those alive plus any other
#' survivors. Individuals whose status is NA in either census are deleted from
#' all calculations.
#'
#'
#' @param quiet Use `TRUE` to suppress messages.
#' @param census1,census2 Two census tables, each being a dataframe and in
#'   particular, a ForestGEO tree table. You may use a stem table, but you more
#'   commonly should use a tree table because demography analyses make more
#'   sense at the scale of a tree than at the scale of stems.
#' @param split1,split2 Optional vectors (column of any one the census
#'   dataframe) to aggregate results by. Defaults to aggregating across the
#'   entire census datasets.
#' @param alivecode Character, codes of the variable `status` that indicate the
#'   tree is alive. The default 'A' is the standard CTFS designation for living
#'   trees or stems.
#' @param mindbh The minimum diameter above which the counts are done. Trees
#'   smaller than `mindbh` are excluded. If `NULL`, all living trees are
#'   included.
#' @param rounddown If `TRUE``, all `dbh < 55` are rounded down to the nearest
#'   multiple of 5.
#' @param method Use "I" to calculate annual dbh increment as
#'   `(dbh2 - dbh1)/time`, or "E" to calculate the relative growth rate as
#'   `(log(dbh2) - log(dbh1)) / time`.
#' @param stdev Logical. Default (`FALSE`) returns confidence limits, otherwise
#'   returns the SD in growth rate per group.
#' @param dbhunit 'cm' or 'mm'.
#' @param growthcol defines how growth is measured, either 'dbh' or 'agb'
#'   (above ground biomass).
#' @param err.limit A number. Numbers such as 10000 are high and will return all
#'   measures.
#' @param maxgrow A number. Numbers such as 10000 are high and will return all
#'   measures.
#'
#' @author Rick Condit, Suzanne Lao.
#'
#' @family demography functions
#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#'
#' @return
#' Metrics of recruitment: Similar to metrics of mortality.
#'
#' Metrics of mortality:
#' * `N`: the number of individuals alive in the census 1 per category
#'   selected.
#' * `D`: the number of individuals no longer alive in census 2.
#' * `rate`: the mean annualized mortality rate constant per category
#'   selected, calculated as (log(N)-log(S))/time.
#' * `upper`: upper confidence limit of mean rate.
#' * `lower`: lower confidence limit of mean rate.
#' * `time`: mean time interval in years.
#' * `date1`: mean date included individuals were measured in census 1, as
#'   julian object (R displays as date, but treats as integer).
#' * `date2`: mean date in census 2.
#' * `dbhmean`: mean dbh in census 1 of individuals included.
#'
#' Metrics of growth:
#' * `rate`, the mean annualized growth rate per category selected, either dbh
#'   increment, or relative growth
#' * `N`, the number of individuals included in the mean (not counting any
#'   excluded)
#' * `clim` (or sd with `stdev = TRUE`), width of confidence interval; add this
#'   number to the mean rate to get upper confidence limit, substract to get
#'   lower
#' * `dbhmean`, mean dbh in census 1 of individuals included
#' * `time`, mean time interval in years
#' * `date1`, mean date included individuals were measured in census 1, as
#'   julian object (R displays as date, but treats as integer)
#' * `date2`, mean date in census 2.
#'
#' @examples
#' census1 <- fgeo.x::tree5
#' census2 <- fgeo.x::tree6
#'
#' recruitment_impl(census1, census2)
#'
#' # Demography by any number of grouping variables via `interaction(...)`
#' sp_quadrat <- interaction(census1$sp, census1$quadrat, quiet = TRUE)
#'
#' recruitment <- recruitment_impl(
#'   census1, census2,
#'   split1 = sp_quadrat,
#'   quiet = TRUE
#' )
#' lapply(recruitment, head)
#'
#' mortality <- mortality_impl(
#'   census1, census2, split1 = sp_quadrat, quiet = TRUE
#' )
#' lapply(mortality, head)
#'
#' growth <- growth_impl(census1, census2, split1 = sp_quadrat, quiet = TRUE)
#' lapply(growth, head)
#'
#' \dontrun{
#' # Convert to convenient dataframes -------------------------------------
#' are_installed <- requireNamespace("fgeo.tool") && requireNamespace("tidyr")
#' if (are_installed) {
#'   library(fgeo.tool)
#'   library(tidyr)
#'
#'   to_df(
#'     recruitment_impl(census1, census2, quiet = TRUE)
#'   )
#'
#'   to_df(mortality)
#'
#'   separate(
#'     to_df(growth),
#'     groups, into = c("sp", "quadrat")
#'   )
#' }
#' }
#' @name demography_impl
NULL

# Recruitment -------------------------------------------------------------

#' @rdname demography_impl
#' @export
recruitment_impl <- function(census1,
                             census2,
                             mindbh = NULL,
                             alivecode = NULL,
                             split1 = NULL,
                             split2 = NULL,
                             quiet = FALSE) {
  # Used before return
  .split2 <- split2

  prep <- wrap_prepare_recruitment_mortality(
    census1, census2, split1, split2, quiet, mindbh
  )
  census1 <- prep$census1
  census2 <- prep$census2
  split1 <- prep$split1
  split2 <- prep$split2
  inc <- prep$inc
  time <- prep$time

  check_alivecode(census1, census2, alivecode, quiet)
  alivecode <- alivecode %||% c("A", "AB", "AS")
  mindbh <- mindbh %||% 0

  survivor <- alive1 <- alive2 <- rep(FALSE, length(census1$status))
  alive1[census1$status == "A"] <- TRUE
  for (i in 1:length(alivecode)) {
    survivor[census1$status == "A" & census2$status == alivecode[i]] <- TRUE
    alive2[census2$status == alivecode[i]] <- TRUE
  }

  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))

  S.inc <- survivor & (census1$dbh >= mindbh)
  N2.inc <- (alive2 & (census2$dbh >= mindbh)) | S.inc

  splitS <- list(split1[S.inc], split2[S.inc])
  splitN <- list(split1[alive1], split2[alive1])
  splitN2 <- list(split1[N2.inc], split2[N2.inc])

  S <- tapply(census2$dbh[S.inc], splitS, length)
  N2 <- tapply(census2$dbh[N2.inc], splitN2, length)
  timeint <- tapply(time[N2.inc], splitN2, mean, na.rm = T)
  startdate <- tapply(census1$date[alive1], splitN, mean, na.rm = T)
  enddate <- tapply(census2$date[N2.inc], splitN2, mean, na.rm = T)

  S <- fill.dimension(S, class1, class2)
  N2 <- fill.dimension(N2, class1, class2)
  timeint <- fill.dimension(timeint, class1, class2, fill = NA)
  startdate <- fill.dimension(startdate, class1, class2, fill = NA)
  enddate <- fill.dimension(enddate, class1, class2, fill = NA)

  if (equal(sum(N2), 0)) {
    early <- list(
      N2 = rep(NA, length(class1)),
      R = rep(NA, length(class1)),
      rate = rep(NA, length(class1)),
      lower = rep(NA, length(class1)),
      upper = rep(NA, length(class1)),
      time = rep(NA, length(class1)),
      date1 = rep(NA, length(class1)),
      date2 = rep(NA, length(class1))
    )
    return(early)
  }

  lower.ci <- upper.ci <- N2
  lower.ci <- find.climits(as.matrix(N2), as.matrix(S), kind = "lower")
  upper.ci <- find.climits(as.matrix(N2), as.matrix(S), kind = "upper")

  rec.rate <- (log(N2) - log(S)) / timeint
  upper.rate <- (log(N2) - log(lower.ci)) / timeint
  lower.rate <- (log(N2) - log(upper.ci)) / timeint

  rec.rate[S == 0] <- upper.rate[S == 0] <- Inf
  upper.rate[lower.ci == 0] <- Inf
  rec.rate[N2 == 0] <- lower.rate[N2 == 0] <- upper.rate[N2 == 0] <- NA

  result <- list(
    N2 = drp(N2),
    R = drp(N2 - S),
    rate = drp(rec.rate),
    lower = drp(lower.rate),
    upper = drp(upper.rate),
    time = drp(timeint),
    date1 = drp(startdate),
    date2 = drp(enddate)
  )

  new_demography_impl(result, .split2)
}

# Mortality ---------------------------------------------------------------

#' @rdname demography_impl
#' @export
mortality_impl <- function(census1,
                           census2,
                           alivecode = NULL,
                           split1 = NULL,
                           split2 = NULL,
                           quiet = FALSE) {
  # Used before return
  .split2 <- split2
  prep <- wrap_prepare_recruitment_mortality(
    census1, census2, split1, split2, quiet,
    mindbh = NULL
  )
  census1 <- prep$census1
  census2 <- prep$census2
  split1 <- prep$split1
  split2 <- prep$split2
  inc <- prep$inc
  time <- prep$time

  check_alivecode(census1, census2, alivecode, quiet)
  alivecode <- alivecode %||% c("A", "AB", "AS")

  alive1 <- alive2 <- rep(FALSE, dim(census1)[1])
  alive1[census1$status == "A"] <- TRUE
  for (i in 1:length(alivecode)) alive2[census2$status == alivecode[i]] <- TRUE

  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))

  splitN <- list(split1[alive1], split2[alive1])
  splitS <- list(split1[alive1 & alive2], split2[alive1 & alive2])

  N <- tapply(census1$dbh[alive1], splitN, length)
  S <- tapply(census1$dbh[alive1 & alive2], splitS, length)
  meantime <- tapply(time[alive1], splitN, mean, na.rm = T)
  meandbh <- tapply(census1$dbh[alive1], splitN, mean, na.rm = T)
  startdate <- tapply(census1$date[alive1], splitN, mean, na.rm = T)
  enddate <- tapply(census2$date[alive1], splitN, mean, na.rm = T)

  N <- fill.dimension(N, class1, class2)
  S <- fill.dimension(S, class1, class2)
  meantime <- fill.dimension(meantime, class1, class2, fill = NA)
  meandbh <- fill.dimension(meandbh, class1, class2, fill = NA)
  startdate <- fill.dimension(startdate, class1, class2, fill = NA)
  enddate <- fill.dimension(enddate, class1, class2, fill = NA)

  if (equal(sum(N), 0)) {
    message(
      "To improve tests, please explain what triggered this message ",
      "(maurolepore@gmail.com)."
    )

    return(list(
      N = rep(NA, length(class1)), D = rep(NA, length(class1)),
      rate = rep(NA, length(class1)),
      lower = rep(NA, length(class1)), upper = rep(NA, length(class1)),
      time = rep(NA, length(class1)), dbhmean = rep(NA, length(class1)),
      date1 = rep(NA, length(class1)), date2 = rep(NA, length(class1))
    ))
  }

  m <- mortality.calculation(
    N = as.matrix(N), S = as.matrix(S), meantime = as.matrix(meantime)
  )

  # ord=order(drp(meandbh))
  result <- list(
    N = drp(m$N),
    D = drp(m$D),
    rate = drp(m$rate),
    lower = drp(m$lowerCI),
    upper = drp(m$upperCI),
    time = drp(m$time),
    date1 = drp(startdate),
    date2 = drp(enddate),
    dbhmean = drp(meandbh)
  )

  new_demography_impl(result, .split2)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
mortality.calculation <- function(N, S, meantime) {
  lower.ci <- find.climits(N, (N - S), kind = "lower")
  upper.ci <- find.climits(N, (N - S), kind = "upper")

  mort.rate <- (log(N) - log(S)) / meantime
  upper.rate <- (log(N) - log(N - upper.ci)) / meantime
  lower.rate <- (log(N) - log(N - lower.ci)) / meantime

  mort.rate[S == 0] <- upper.rate[S == 0] <- Inf
  upper.rate[upper.ci == N] <- Inf
  lower.rate[lower.ci == N] <- 0
  mort.rate[N == 0] <- lower.rate[N == 0] <- upper.rate[N == 0] <- NA

  out <- list(
    N = N,
    S = S,
    D = N - S,
    rate = mort.rate,
    lowerCI = lower.rate,
    upperCI = upper.rate,
    time = meantime
  )

  # FIXME: Returning different data structures is error-prone
  if (is.null(dim(N))) {
    out <- data.frame(out)
  }

  out
}

# Growth ------------------------------------------------------------------

#' @rdname demography_impl
#' @export
growth_impl <- function(census1,
                        census2,
                        rounddown = FALSE,
                        method = "I",
                        stdev = FALSE,
                        dbhunit = "mm",
                        mindbh = NULL,
                        growthcol = "dbh",
                        err.limit = 1000,
                        maxgrow = 1000,
                        split1 = NULL,
                        split2 = NULL,
                        quiet = FALSE) {
  # Used before return
  .split2 <- split2

  # More crucial names checked downstream
  lapply(list(census1, census2), check_crucial_names, "stemID")

  # TODO: check_method

  prep_d <- prepare_demography(census1, census2, split1, split2, quiet, mindbh)
  census1 <- prep_d$census1
  census2 <- prep_d$census2
  mindbh <- prep_d$mindbh
  split1 <- prep_d$split1
  split2 <- prep_d$split2

  if (is.null(mindbh)) mindbh <- 0

  size1 <- census1[[growthcol]]
  size2 <- census2[[growthcol]]

  if (is.null(split1)) {
    split1 <- rep("all", dim(census1)[1])
  }
  if (is.null(split2)) {
    split2 <- rep("all", dim(census2)[1])
  }
  if (is.null(census2$codes)) {
    # This could be just `census2$codes <- "."`
    census2$codes <- rep(".", length(size2))
  }

  time <- time_diff(census1, census2)

  if (rounddown) {
    sm <- ((size1 < 55 | size2 < 55) & !is.na(size1) & !is.na(size2))
    size1[sm] <- rndown5(size1[sm])
    size2[sm] <- rndown5(size2[sm])
  }

  if (method == "I") {
    growthrate <- (size2 - size1) / time
  } else if (method == "E") {
    growthrate <- (log(size2) - log(size1)) / time
  }

  good <- trim.growth(census1, census2, time,
    err.limit = err.limit,
    maxgrow = maxgrow, mindbh = mindbh
  )
  growthrate[!good] <- NA
  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))
  splitgood <- list(split1[good], split2[good])
  mean.grow <- tapply(growthrate[good], splitgood, mean, na.rm = TRUE)
  sd.grow <- tapply(growthrate[good], splitgood, sd, na.rm = TRUE)
  N <- tapply(growthrate[good], splitgood, length)
  meandbh <- tapply(census1$dbh[good], splitgood, mean, na.rm = TRUE)
  meansize <- tapply(size1[good], splitgood, mean, na.rm = TRUE)
  interval <- tapply(time[good], splitgood, mean, na.rm = TRUE)
  startdate <- tapply(census1$date[good], splitgood, mean, na.rm = TRUE)
  enddate <- tapply(census2$date[good], splitgood, mean, na.rm = TRUE)

  mean.grow <- fill.dimension(mean.grow, class1, class2, fill = NA)
  N <- fill.dimension(N, class1, class2, fill = 0)
  meandbh <- fill.dimension(meandbh, class1, class2, fill = NA)
  interval <- fill.dimension(interval, class1, class2, fill = NA)
  startdate <- fill.dimension(startdate, class1, class2, fill = NA)
  enddate <- fill.dimension(enddate, class1, class2, fill = NA)
  sd.grow <- fill.dimension(sd.grow, class1, class2, fill = NA)
  ci.grow <- sd.grow
  ci.grow[N == 0] <- NA
  ci.grow[N > 0] <- sd.grow[N > 0] * qt(0.975, N[N > 0]) / sqrt(N[N > 0])
  result <- list(
    rate = drp(mean.grow),
    N = drp(N),
    sd = drp(sd.grow),
    dbhmean = drp(meandbh),
    time = drp(interval),
    date1 = drp(startdate),
    date2 = drp(enddate)
  )

  if (!stdev) {
    result$sd <- NULL
    result <- append(result, list(clim = drp(ci.grow)), after = 2)
  }

  new_demography_impl(result, .split2)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
trim.growth <- function(cens1,
                        cens2,
                        time,
                        slope = 0.006214,
                        intercept = 0.9036,
                        err.limit = 4,
                        maxgrow = 75,
                        pomcut = 0.05,
                        mindbh = 10,
                        dbhunit = "mm",
                        exclude.stem.change = TRUE) {
  if (dbhunit == "cm") {
    intercept <- intercept / 10
  }
  stdev.dbh1 <- slope * cens1$dbh + intercept
  growth <- (cens2$dbh - cens1$dbh) / time
  bad.neggrow <- which(cens2$dbh <= (cens1$dbh - err.limit *
      stdev.dbh1))
  bad.posgrow <- which(growth > maxgrow)
  homdiff <- abs(as.numeric(cens2$hom) - as.numeric(cens1$hom)) / as.numeric(cens1$hom)
  accept <- rep(TRUE, length(cens1$dbh))
  accept[homdiff > pomcut] <- FALSE
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE
  if (exclude.stem.change) {
    accept[cens1$stemID != cens2$stemID] <- FALSE
  }
  accept[cens1$dbh < mindbh] <- FALSE
  accept[is.na(cens1$dbh) | is.na(cens2$dbh) | cens2$dbh <=
      0] <- FALSE
  return(accept)
}



# Helpers -----------------------------------------------------------------

new_demography_impl <- function(.x, split2, ...) {
  stopifnot(is.list(.x))
  .x <- structure(.x, class = c("demography_impl", class(.x)))

  # Flag deprecated argument to be used in fgeo.tool::to_df()
  if (!is.null(split2)) {
    attr(.x, "split2") <- TRUE
  }

  .x
}

#' @keywords internal
#' @export
#' @noRd
print.demography_impl <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}

# Wrap code shared by recruitment_impl() and mortality_impl() including what's
# also shared by growth_impl.
wrap_prepare_recruitment_mortality <- function(census1,
  census2,
  split1,
  split2,
  quiet,
  mindbh) {
  prep1 <- prepare_demography(census1, census2, split1, split2, quiet, mindbh)
  prep2 <- prepare_recr_mort(
    prep1$census1, prep1$census2, prep1$split1, prep1$split2
  )
  list(
    census1 = prep2$census1,
    census2 = prep2$census2,
    split1 = prep2$split1,
    split2 = prep2$split2,
    inc = prep2$inc,
    time = prep2$time
  )
}

# Code shared by recruitment_impl(), mortality_impl() and growth().
prepare_demography <- function(census1,
  census2,
  split1,
  split2,
  quiet,
  mindbh = NULL) {
  force(census1)
  force(census2)
  check_prepare_demography(census1, census2, mindbh, split1, split2, quiet)

  # Avoid tibbles, via as.data.frame(<maybe-tibble>). Legacy code relies on the
  # default behaviour of `[` by which single-column dataframes are automatically
  # simplified to vectors via the default `drop = TRUE` (as in `[row, col, drop
  # = TRUE]`). But tibbles conservatively default to `drop = FALSE`. To avoid
  # conflicts with legacy code downstream I preserve the original behaviour and
  # treat tibbles as.data.frame(). Yet I fix the unsafe code wherever I find it.
  census1 <- as.data.frame(census1, stringsAsFactors = FALSE)
  census2 <- as.data.frame(census2, stringsAsFactors = FALSE)

  if (!quiet) inform_dbh_range(census1, census2, mindbh)

  if (all(is.na(census1$date)) || all(is.na(census1$date))) {
    stop("Can't use `date`; all values are all missing.", call. = FALSE)
  }

  list(
    census1 = census1,
    census2 = census2,
    mindbh = mindbh,
    split1 = split1,
    split2 = split2
  )
}

# Code shared by recruitment_impl() and mortality_impl() but not growth().
prepare_recr_mort <- function(census1, census2, split1, split2) {
  split1 <- split1 %||% rep("all", dim(census1)[1])
  split2 <- split2 %||% rep("all", dim(census2)[1])

  # Pick non-missing status (not NA and not M -- which is short for missing)
  inc <- !is.na(census1$status) &
    !is.na(census2$status) &
    census1$status != "M" &
    census2$status != "M"

  census1 <- census1[inc, ]
  census2 <- census2[inc, ]
  split1 <- split1[inc]
  split2 <- split2[inc]

  time <- time_diff(census1, census2)

  list(
    census1 = census1,
    census2 = census2,
    split1 = split1,
    split2 = split2,
    inc = inc,
    time = time
  )
}

time_diff <- function(census1, census2) {
  check_crucial_names(census1, "date")
  check_crucial_names(census2, "date")
  time <- (census2$date - census1$date) / 365.25
  warn_if_time_diff_is_na(time)
  warn_if_time_diff_is_cero(time)
  time
}

check_prepare_demography <- function(census1,
  census2,
  mindbh,
  split1,
  split2,
  quiet) {
  crucial <- c("dbh", "hom", "status", "date")
  check_crucial_names(census1, crucial)
  check_crucial_names(census2, crucial)

  if (!is.null(split2)) {
    warn_once(
      paste0(
        "`split2` is deprecated.\n",
        "* Bad: `split1 = x1, split2 = x2`\n",
        "* Good: `split1 = interaction(x1, x2)`"
      )
    )
  }

  invisible()
}

check_alivecode <- function(census1, census2, alivecode, quiet) {
  check_crucial_names(census1, c("status"))
  check_crucial_names(census2, c("status"))
  # User provides `alivecode`
  if (!is.null(alivecode) && !quiet) {
    inform_once(
      "`alivecode` is deprecated. Consider filtering the data before."
    )
  }
  statuses <- unique(c(census1$status, census2$status))
  if (!is.null(alivecode) && !any(alivecode %in% statuses)) {
    warning(
      "`alivecode` matches no value of `status` (", glue_comma(statuses), ").",
      call. = FALSE
    )
  }
}

inform_dbh_range <- function(census1, census2, mindbh) {
  mindbh <- mindbh %||% 0
  message(
    "Detected dbh ranges:\n",
    "  * `census1` = ", glue_dash(range(census1$dbh, na.rm = TRUE)), ".\n",
    "  * `census2` = ", glue_dash(range(census2$dbh, na.rm = TRUE)), ".\n",
    paste0("Using dbh `mindbh = ", mindbh, "` and above.\n")
  )

  invisible(list(census1 = census1, census2 = census2))
}

warn_if_time_diff_is_cero <- function(time) {
  time_difference_is_cero <- identical(unique(time[!is.na(time)]), 0)
  if (time_difference_is_cero) {
    warning(
      "Time difference is cero. Are you using two difference censuses?",
      call. = FALSE
    )
  }
  invisible(time)
}

warn_if_time_diff_is_na <- function(time) {
  if (all(is.na(time))) {
    warning(
      "Time-difference are all missing values. Check date variables.",
      call. = FALSE
    )
  }
  invisible(time)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
find.climits <- function(N, D, alpha = .05, kind = "upper") {
  if (kind == "lower") {
    result <- N * (1 - qbeta(1 - alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == 0] <- 0
  }
  else if (kind == "upper") {
    result <- N * (1 - qbeta(alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == N] <- N[D == N]
  }

  return(result)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
drp <- function(x) {
  return(drop(as.matrix(x)))
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
fill.dimension <- function(dataarray, class1, class2, fill = 0) {
  result <- matrix(fill, nrow = length(class1), ncol = length(class2))
  rownames(result) <- class1
  result <- data.frame(result, stringsAsFactors = FALSE)
  names(result) <- class2
  result[rownames(dataarray), colnames(dataarray)] <- dataarray
  result[is.na(result)] <- fill
  return(result)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
#' @author Rick Condit, Suzanne Lao.
rndown5 <- function(s) 5 * floor(s / 5)

