commas <- function(...) paste0(..., collapse = ", ")
dashes <- function(x) paste0(x, collapse = "-")

equal <- function(x, y) isTRUE(all.equal(x, y))

abort_bad_class <- function(x) {
  .class <- glue_collapse(class(x), sep = ", ", last = ", or ")
  abort(glue("Can't deal with data of class: {.class}."))
}

# Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

# From https://github.com/...
# * tidyverse/tibble/blob/81e736e72716ad1b2f406e52c7d36ded202820da/R/utils.r#L71
# * r-lib/rlang/blob/cd272fdea1d84c596ba63e05c8f79645bbf03767/R/cnd.R#L936
warn_once_env <- new.env(parent = emptyenv())
warn_once <- function(msg) {
  if (exists(msg, warn_once_env)) return(invisible())
  warning(
    paste0(msg, "\nThis warning is displayed once per session."),
    call. = FALSE
  )
  warn_once_env[[msg]] <- TRUE
  invisible()
}
forget_warn_once <- function() {
  rm(list = ls(warn_once_env), pos = warn_once_env)
}

inform_once_env <- new.env(parent = emptyenv())
inform_once <- function(msg) {
  if (exists(msg, inform_once_env)) return(invisible())
  message(paste0(msg, "\nThis message is displayed once per session."))
  inform_once_env[[msg]] <- TRUE
  invisible()
}
forget_inform_once <- function() {
  rm(list = ls(inform_once_env), pos = inform_once_env)
}
