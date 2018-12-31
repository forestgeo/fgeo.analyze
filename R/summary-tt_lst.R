#' Summary objects of class "tt_lst".
#'
#' This method helps you to interpret the results of `tt_test()` (which
#' outputs objects of class "tt_lst").
#'
#' @param object The output of [tt_test()], i.e. an S3 object of class tt_lst.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [tt_test()].
#' @return A dataframe.
#'
#' @author Daniel Zuleta.
#'
#' @examples
#' census <- fgeo.x::tree6_3species
#' habitat <- fgeo.x::habitat
#'
#' summary(
#'   tt_test(census, habitat)
#' )
#'
#' @family habitat functions
#' @family methods for common generics
#' @export
summary.tt_lst <- function(object, ...) {
  ttdf <- as.data.frame(do.call(rbind, object))
  habitats_n <- dim(ttdf)[2] / 6

  # FIXME: Super slow. Should be as long as the output
  result <- data.frame()

  # FIXME: seq_along() is safer than 1:length(x)
  for (species in 1:dim(ttdf)[1]) {
    # FIXME: seq_along() is safer than 1:length(x)
    for (habitat in 1:habitats_n) {
      lhs <- ttdf[species, (habitat * 6) - 1]
      rhs <- ttdf[species,  habitat * 6]
      limit <- 0.05

      cond1 <- lhs ==  1 & (1 - (rhs)) <  limit
      cond2 <- lhs ==  1 & (1 - (rhs)) >= limit
      cond3 <- lhs == -1 &      (rhs)  <  limit
      cond4 <- lhs == -1 &      (rhs)  >= limit

      # TODO: Nested ifelse() are diffucult to read. Try different approach
      result[species, habitat] <- ifelse(
        cond1, "aggregated", ifelse(
          cond2, "agg_nonsignificant", ifelse(
            cond3, "repelled", ifelse(
              cond4, "rep_nonsignificant", "neutral"))))
    }
  }

  result <- as_tibble(dplyr::bind_cols(species = row.names(ttdf), result))
  result[] <- lapply(result, as.character)

  set_names(result, c("species", paste0("habitat_", seq_len(habitats_n))))
}
