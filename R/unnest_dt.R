unnest_dt <- function(dt, col, id, id_vars = NULL) {
  stopifnot(is.data.table(dt))

  if (missing(id_vars)) {
    by <-substitute(id)
    col <-substitute(unlist(col, recursive = FALSE))

    dt[, eval(col), by = eval(by)]
  } else {
    col <-substitute(unlist(col, recursive = FALSE))
    dt[, eval(col), by = id_vars]
  }
}
