#' Summary of `tt_test()` results.
#'
#' @param object An object of class "tt_df" or "tt_lst".
#' @param ... Not used (included only for compatibility with `summary`).
#'
#' @author Adapted from code contributed by Daniel Zuleta.
#'
#' @return A tibble.
#'
#' @seealso [tt_test()], [base::summary()].
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' tt_result <- tt_test(fgeo.x::tree6_3species, fgeo.x::habitat)
#'
#' summary(tt_result)
#'
#' # Same
#' summary(as_tibble(tt_result))
#'
#' # You may want to add the explanation to the result of `tt_test()`
#'
#' dplyr::left_join(as_tibble(tt_result), summary(tt_result))
#'
#' # You may prefer a wide matrix
#' Reduce(rbind, tt_result)
#'
#' # You may prefer a wide dataframe
#' tidyr::spread(summary(tt_result), "habitat", "association")
#' @family methods for common generics
#' @export
summary.tt_df <- function(object, ...) {
  out <- mutate(
    object,
    # Short alias to fit in screen-width
    obs = .data$Obs.Quantile,
    association =   dplyr::case_when(
      (.data$obs - 1) ==  1 & (1 - (.data$obs)) <  0.05 ~ "aggregated",
      (.data$obs - 1) ==  1 & (1 - (.data$obs)) >= 0.05 ~ "agg_nonsignificant",
      (.data$obs - 1) == -1 &      (.data$obs)  <  0.05 ~ "repelled",
      (.data$obs - 1) == -1 &      (.data$obs)  >= 0.05 ~ "rep_nonsignificant",
      TRUE                                              ~ "neutral"
    )
  )

  arrange(out, .data$sp, .data$habitat)[c("sp", "habitat", "association")]
}

#' @rdname summary.tt_df
#' @export
summary.tt_lst <- function(object, ...) {
  summary.tt_df(as_tibble(object))
}
