#' Title
#'
#' @param dt A dt
#' @param col Quoted name of column to expand by
#' @param group Quoted name of column to group by
#' @param step Steps by which to pad the column specified in \code{col}
#' @param fill Value to fill other columns with
#' @param min The minimum value \code{col} should take
#' @param max The maximum value \code{col} should take. If none is specified the max value of \code{col} will be used. If the number of rows in the final dt will surpass 1 000 000 the user will be given the option to provide a max in terminal if \code{ask = TRUE}.
#' @param ask Should the user be asked to provide a max if final dt will be > 1 000 000 long?
#'
#' @return A dt with all combinations if \code{col} & \code{group} with restrictions as described above. All columns in new rows other than \code{col} & \code{group} will be filled with 0 unless specified otherwise.
#' @export
#'
dtextend <- function(dt,
                     col,
                     group,
                     step,
                     fill = 0,
                     min = 0,
                     max = "auto",
                     ask = TRUE)
{
  if (!col %in% colnames(dt)) {stop("Col not in dt")}

  if (!all(is.na(group))) {
    groups <- dt[, ..group][,unique(.SD)][[1]]
  } else {groups <- "tmp"}


  if (max == "auto") { # Get max in col, if dt will have more than 1000000 rows, ask for new max
    rm(max)
    max <- do.call(max, dt[, ..col])
    if (max / step > 1000000 & ask == TRUE) {
      message(paste0("This will result in a dt with ", max * length(groups) / step, " rows. Input a new max? (turn off this message with ask = FALSE)\n(y/n)"))
        ans <- readline()
        while (!ans %in% c("y", "n")) {
          message("Please input y or n")
          ans <- readline()
        }
        if (ans == "y") {
          ans <- readline(prompt = "New max: ")
          while (grepl("\\D", ans)) {
            message("Please input a number")
            ans <- readline(prompt = "New max: ")
          }
          max <- as.numeric(ans)
        }
      }
    }


  ref <- seq.int(from = min, to = max, by = step)

  res <- as.data.table(expand.grid(groups, ref))

  colnames(res) <- c(eval(group), eval(col))

  if (all(is.na(group))) {
    dt[, "tmp" := 1]
  }

  res <- dt[
            res,
            on = c(eval(group), eval(col)),

          ]
  if (!is.na(fill)) {
    res[is.na(res),] <- fill
  }

  setorderv(res, cols = c(eval(group), eval(col)))

  if (all(is.na(group))) {
    dt[, tmp := NULL]
  }

  return(res)
}

