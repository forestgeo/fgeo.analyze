#' Summary objects of class "tt_lst".
#' 
#' This method helps you to interpret the results of `tt_test()` (which
#' outputs objects of class "tt_lst").
#' 
#' @param object The output of [tt_test()], i.e. an S3 object of class tt_lst.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [tt_test()].
#' 
#' @family methods for common generics
#'
#' @author Daniel Zuleta.
#'
#' @return A dataframe.
#' @export
summary.tt_lst <- function(object, ...) {
  ttdf <- as.data.frame(do.call(rbind, object))
  habitats_n <- dim(ttdf)[2] / 6
  out <- data.frame()
  
  for (species in 1:dim(ttdf)[1]) {
    for (habitat in 1:habitats_n) {
      lhs <- ttdf[species, (habitat * 6) - 1]
      rhs <- ttdf[species,  habitat * 6]
      limit <- 0.05

      cond1 <- lhs ==  1 & (1 - (rhs)) <  limit
      cond2 <- lhs ==  1 & (1 - (rhs)) >= limit
      cond3 <- lhs == -1 &      (rhs)  <  limit
      cond4 <- lhs == -1 &      (rhs)  >= limit
      
      out[species, habitat] <- ifelse(
        cond1, "aggregated", ifelse(
          cond2, "agg_nonsignificant", ifelse(
            cond3, "repelled", ifelse(
              cond4, "rep_nonsignificant", "neutral"))))
    }
  }

  out <- data.frame(cbind(Species = row.names(ttdf), out))
  out[] <- lapply(out, as.character)

  names(out) <- c("Species", paste0("Habitat_", seq_len(habitats_n)))
  out
}
