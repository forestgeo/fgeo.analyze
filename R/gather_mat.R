#' Gather matrix columns into a long-format dataframe.
#'
#' These functions convert a matrix to dataframes with three-columns: One for
#' the matrix-rownames; a second one for the matrix-colnames; and a third one
#' for the matrix-values. These functions combine features of `tidyr::gather()`
#' and `tibble::enframe()`:
#' * [gather_mat()] outputs a long-format dataframe.
#' * [gather_mats()]
#' returns a list of dataframes where each element of the list maps to a column
#' of the original matrix.
#'
#' @param mat A matrix.
#' @param rownm,colnm,value Names of the columns that store the row names, the
#'   column names and the values.
#' @param ... Arguments passed to gather_mats.
#'
#' @seealso `enframe()` (__tibble__ package) and `gather()` (__tidyr__ package).
#' @return
#' * [gather_mat()]: A dataframe.
#' * [gather_mats()]: A list of dataframes.
#'
#' @examples
#' mat <- matrix(1:6, 2, dimnames = list(LETTERS[1:2], letters[1:3]))
#' mat
#' gather_mats(mat)
#' gather_mat(mat)
#' gather_mat(mat, "metric", "sp")
#' mat <- matrix(1:6, 2)
#' mat
#' gather_mat(mat)
#' @family general functions to construct or restructure data
#' @noRd
gather_mat <- function(mat,
                       rownm = "rownames",
                       colnm = "colnames",
                       value = "value") {
  Reduce(
    rbind,
    gather_mats(mat = mat, rownm = rownm, colnm = colnm, value = value)
  )
}

gather_mats <- function(mat,
                        rownm = "rownames",
                        colnm = "colnames",
                        value = "value") {
  stopifnot(!is.null(mat))

  if (is.null(rownames(mat))) {
    rownames(mat) <- 1:nrow(mat)
  }
  if (is.null(colnames(mat))) {
    colnames(mat) <- 1:ncol(mat)
  }
  rvar <- rownames(mat)
  cvar <- colnames(mat)
  cols <- lapply(data.frame(mat), function(x) x)
  for (i in seq_along(cvar)) {
    df <- list(rvar, cvar[[i]], unname(mat[, cvar[[i]]]))
    names(df) <- c(rownm, colnm, value)
    cols[[i]] <- as.data.frame(df, stringsAsFactors = FALSE)
  }
  cols
}
