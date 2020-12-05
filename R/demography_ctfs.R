#' Recruitment, mortality, and growth.
#'
#' @description
#' These functions are adapted from the CTFS-R package. Compared to the
#' original functions, these ones have a similar interface but use more
#' conservative defaults and allow suppressing messages. These functions also
#' feature formal tests, bug fixes, additional assertions, and improved
#' messages.
#'
#' @details
#' Survivors are all individuals alive in both censuses, with `status == A` in
#' the first census, and a diameter greater than `mindbh` in the first census.
#' The total population in the second census includes all those alive plus any
#' other survivors. Individuals whose status is `NA` in either census are
#' deleted from all calculations.
#'
#' @param census1,census2 Two census tables, each being a ForestGEO-like _tree_
#'   table (dataframe). A _stem_ table won't fail, but you should use a _tree_
#'   table because demography analyses make more sense at the scale of trees
#'   than at the scale of stems.
#' @param mindbh The minimum diameter above which the counts are done. Trees
#'   smaller than `mindbh` are excluded. By default all living trees of any size
#'   are included.
#' @param alivecode Character; valid values of `status` indicating that a tree
#'   is alive. The default, 'A', is the standard CTFS designation for living
#'   trees or stems.
#' @param split1,split2 Optional vector(s) to aggregate results by. Each vector
#'   should be a column of either `census1` or `census2`. The default aggregates
#'   the result across the entire census datasets.
#' @param quiet Use `TRUE` to suppress messages.
#' @param rounddown If `TRUE`, all `dbh < 55` are rounded down to the nearest
#'   multiple of 5.
#' @param method Either "I" or "E":
#'   * Use "I" to calculate annual dbh increment as `(dbh2 - dbh1)/time`
#'   * Use "E" to calculate the relative growth rate as
#'   `(log(dbh2) - log(dbh1)) / time`
#' @param stdev Logical:
#'    * `FALSE` returns confidence limits.
#'    * `TRUE` returns the SD in growth rate per group.
#' @param dbhunit "cm" or "mm".
#' @param growthcol Either "dbh" or "agb" to define how growth is measured.
#' @param err.limit,maxgrow A number. Numbers such as 10000 are high and will
#'   return all measures.
#'
#' @author Richard Condit, Suzanne Lao.
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
#'   increment, or relative growth.
#' * `N`, the number of individuals included in the mean (not counting any
#'   excluded).
#' * `clim` (or sd with `stdev = TRUE`), width of confidence interval; add this
#'   number to the mean rate to get upper confidence limit, substract to get
#'   lower.
#' * `dbhmean`, mean dbh in census 1 of individuals included.
#' * `time`, mean time interval in years.
#' * `date1`, mean date included individuals were measured in census 1, as
#'   julian object (R displays as date, but treats as integer).
#' * `date2`, mean date in census 2.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' census1 <- fgeo.x::tree5
#' census2 <- fgeo.x::tree6
#'
#' as_tibble(
#'   recruitment_ctfs(census1, census2)
#' )
#'
#' # Use `interaction(...)` to aggregate by any number of grouping variables
#' sp_quadrat <- interaction(census1$sp, census1$quadrat)
#'
#' recruitment <- recruitment_ctfs(
#'   census1, census2,
#'   split1 = sp_quadrat,
#'   quiet = TRUE
#' )
#' as_tibble(recruitment)
#'
#' mortality <- mortality_ctfs(
#'   census1, census2,
#'   split1 = sp_quadrat, quiet = TRUE
#' )
#' as_tibble(mortality)
#'
#' growth <- growth_ctfs(census1, census2, split1 = sp_quadrat, quiet = TRUE)
#' as_tibble(growth)
#'
#' # Easy way to separate grouping variables
#' tidyr_is_installed <- requireNamespace("tidyr", quietly = TRUE)
#' if (tidyr_is_installed) {
#'   library(tidyr)
#'
#'   as_tibble(growth) %>%
#'     separate(groups, into = c("sp", "quadrat"))
#' }
#' @family demography functions
#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#' @export
recruitment_ctfs <- function(census1,
                             census2,
                             mindbh = NULL,
                             alivecode = NULL,
                             split1 = NULL,
                             split2 = NULL,
                             quiet = FALSE) {
  prep <- wrap_prepare_recruitment_mortality(
    census1, census2, split1, split2, quiet, mindbh
  )

  check_alivecode(prep$census1, prep$census2, alivecode, quiet)
  alivecode <- alivecode %||% c("A", "AB", "AS")
  mindbh <- mindbh %||% 0

  survivor <- alive1 <- alive2 <- rep(FALSE, length(prep$census1$status))
  alive1[prep$census1$status == "A"] <- TRUE
  for (i in 1:length(alivecode)) {
    survivor[prep$census1$status == "A" & prep$census2$status == alivecode[i]] <- TRUE
    alive2[prep$census2$status == alivecode[i]] <- TRUE
  }

  class1 <- sort(unique(prep$split1))
  class2 <- sort(unique(prep$split2))
  S.inc <- survivor & (prep$census1$dbh >= mindbh)
  N2.inc <- (alive2 & (prep$census2$dbh >= mindbh)) | S.inc
  splitS <- list(prep$split1[S.inc], prep$split2[S.inc])
  splitN <- list(prep$split1[alive1], prep$split2[alive1])
  splitN2 <- list(prep$split1[N2.inc], prep$split2[N2.inc])

  fill_0 <- fill_with_classes(list(class1, class2), fill = 0)
  fill_NA <- fill_with_classes(list(class1, class2), fill = NA)
  S <- fill_0(apply_length(prep$census2$dbh[S.inc], splitS))
  N2 <- fill_0(apply_length(prep$census2$dbh[N2.inc], splitN2))
  timeint <- fill_NA(apply_mean(prep$time[N2.inc], splitN2))
  startdate <- fill_NA(apply_mean(prep$census1$date[alive1], splitN))
  enddate <- fill_NA(apply_mean(prep$census2$date[N2.inc], splitN2))

  if (equal(sum(N2), 0)) {
    nms <- c("N2", "R", "rate", "lower", "upper", "time", "date1", "date2")
    result <- Map(function(x) rep(NA, length(class1)), nms)
    return(new_demography_ctfs(result, split2))
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
  new_demography_ctfs(result, split2)
}

#' @rdname recruitment_ctfs
#' @export
mortality_ctfs <- function(census1,
                           census2,
                           alivecode = NULL,
                           split1 = NULL,
                           split2 = NULL,
                           quiet = FALSE) {
  prep <- wrap_prepare_recruitment_mortality(
    census1, census2, split1, split2, quiet,
    mindbh = NULL
  )

  check_alivecode(prep$census1, prep$census2, alivecode, quiet)
  alivecode <- alivecode %||% c("A", "AB", "AS")

  alive1 <- alive2 <- rep(FALSE, dim(prep$census1)[1])
  alive1[prep$census1$status == "A"] <- TRUE
  for (i in 1:length(alivecode)) {
    alive2[prep$census2$status == alivecode[i]] <- TRUE
  }

  class1 <- sort(unique(prep$split1))
  class2 <- sort(unique(prep$split2))
  splitN <- list(prep$split1[alive1], prep$split2[alive1])
  splitS <- list(prep$split1[alive1 & alive2], prep$split2[alive1 & alive2])

  fill_0 <- fill_with_classes(list(class1, class2), fill = 0)
  fill_NA <- fill_with_classes(list(class1, class2), fill = NA)
  N <- fill_0(apply_length(prep$census1$dbh[alive1], splitN))
  S <- fill_0(apply_length(prep$census1$dbh[alive1 & alive2], splitS))
  meantime <- fill_NA(apply_mean(prep$time[alive1], splitN))
  meandbh <- fill_NA(apply_mean(prep$census1$dbh[alive1], splitN))
  startdate <- fill_NA(apply_mean(prep$census1$date[alive1], splitN))
  enddate <- fill_NA(apply_mean(prep$census2$date[alive1], splitN))

  if (equal(sum(N), 0)) {
    message(
      "To improve tests, please explain what triggered this message ",
      "(maurolepore@gmail.com)."
    )
    .names <-
      c("N", "D", "rate", "lower", "upper", "time", "dbhmean", "date1", "date2")
    result <- Map(function(x) rep(NA, length(class1)), .names)
    return(new_demography_ctfs(result, split2))
  }

  m <- mortality.calculation(
    N = as.matrix(N), S = as.matrix(S), meantime = as.matrix(meantime)
  )
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
  new_demography_ctfs(result, split2)
}

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
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

  result <- list(
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
    result <- data.frame(result)
  }
  result
}

#' @rdname recruitment_ctfs
#' @export
growth_ctfs <- function(census1,
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
  # More crucial names checked downstream
  lapply(list(census1, census2), check_crucial_names, "stemID")

  prep <- prepare_demography(census1, census2, split1, split2, quiet, mindbh)
  if (is.null(prep$mindbh)) {
    prep$mindbh <- 0
  }
  size1 <- prep$census1[[growthcol]]
  size2 <- prep$census2[[growthcol]]
  if (is.null(prep$split1)) {
    prep$split1 <- rep("all", dim(prep$census1)[1])
  }
  if (is.null(prep$split2)) {
    prep$split2 <- rep("all", dim(prep$census2)[1])
  }
  if (is.null(prep$census2$codes)) {
    prep$census2$codes <- "."
  }
  time <- time_diff(prep$census1, prep$census2)
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

  good <- trim.growth(prep$census1, prep$census2, time,
    err.limit = err.limit,
    maxgrow = maxgrow, mindbh = prep$mindbh
  )
  growthrate[!good] <- NA
  class1 <- sort(unique(prep$split1))
  class2 <- sort(unique(prep$split2))
  splitgood <- list(prep$split1[good], prep$split2[good])

  fill_0 <- fill_with_classes(list(class1, class2), fill = 0)
  fill_NA <- fill_with_classes(list(class1, class2), fill = NA)
  N <- fill_0(apply_length(growthrate[good], splitgood))
  mean.grow <- fill_NA(apply_mean(growthrate[good], splitgood))
  sd.grow <- fill_NA(apply_sd(growthrate[good], splitgood))
  meandbh <- fill_NA(apply_mean(prep$census1$dbh[good], splitgood))
  interval <- fill_NA(apply_mean(time[good], splitgood))
  startdate <- fill_NA(apply_mean(prep$census1$date[good], splitgood))
  enddate <- fill_NA(apply_mean(prep$census2$date[good], splitgood))

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
  new_demography_ctfs(result, split2)
}

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
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
  accept[is.na(cens1$dbh) | is.na(cens2$dbh) | cens2$dbh <= 0] <- FALSE
  accept
}

new_demography_ctfs <- function(.x, split2, ...) {
  stopifnot(is.list(.x))
  .x <- structure(.x, class = c("demography_ctfs", class(.x)))

  # Flag deprecated argument to be used in as_tibble()
  if (!is.null(split2)) {
    attr(.x, "split2") <- TRUE
  }

  .x
}

#' @export
print.demography_ctfs <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}

# Wrap code shared by recruitment_ctfs() and mortality_ctfs() including what's
# also shared by growth_ctfs.
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

# Code shared by recruitment_ctfs(), mortality_ctfs() and growth().
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

# Code shared by recruitment_ctfs() and mortality_ctfs() but not growth().
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

  if (duplicated_treeid(census1) || duplicated_treeid(census2)) {
    warn(glue(
      "The census data should have a single row per tree per census.
      Do you need `fgeo.tool::pick_main_stem()`?"
    ))
  }

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

duplicated_treeid <- function(data) {
  any(duplicated(set_names(data, tolower)$treeid))
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
      "`alivecode` matches no value of `status` (", commas(statuses), ").",
      call. = FALSE
    )
  }
}

inform_dbh_range <- function(census1, census2, mindbh) {
  mindbh <- mindbh %||% 0
  message(
    "Detected dbh ranges:\n",
    "  * `census1` = ", dashes(range(census1$dbh, na.rm = TRUE)), ".\n",
    "  * `census2` = ", dashes(range(census2$dbh, na.rm = TRUE)), ".\n",
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

fill_with_classes <- function(classes, fill) {
  force(classes)
  force(fill)
  function(x) {
    fill.dimension(x, classes[[1]], classes[[2]], fill = fill)
  }
}

apply_mean <- function(X, INDEX) tapply(X, INDEX, FUN = mean, na.rm = TRUE)
apply_sd <- function(X, INDEX) tapply(X, INDEX, FUN = sd, na.rm = TRUE)
apply_length <- function(X, INDEX) tapply(X, INDEX, FUN = length)

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
find.climits <- function(N, D, alpha = .05, kind = "upper") {
  if (kind == "lower") {
    result <- N * (1 - qbeta(1 - alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == 0] <- 0
  }
  else if (kind == "upper") {
    result <- N * (1 - qbeta(alpha / 2, shape1 = N - D + 1, shape2 = D + 1))
    result[D == N] <- N[D == N]
  }

  result
}

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
drp <- function(x) {
  drop(as.matrix(x))
}

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
fill.dimension <- function(dataarray, class1, class2, fill = 0) {
  result <- matrix(fill, nrow = length(class1), ncol = length(class2))
  rownames(result) <- class1

  result <- data.frame(result, stringsAsFactors = FALSE)
  names(result) <- class2
  result[rownames(dataarray), colnames(dataarray)] <- dataarray
  result[is.na(result)] <- fill
  result
}

#' @author Richard Condit, Suzanne Lao.
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @noRd
rndown5 <- function(s) {
  5 * floor(s / 5)
}
