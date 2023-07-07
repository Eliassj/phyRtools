#' Calculate ISI
#'
#' @param x A list-object created by \code{\link{loadSpikes}}
#' @param ISIonly Should a dt containing only cluster and ISI be returned? Defaults to FALSE.
#'
#' @return
#' ISIonly = FALSE -> Modifies the list-object adding a column to spiketimes with ISI.
#' ISIonly = TRUE -> A dt with cluster and ISI for each spike.
#' @export
#'
#' @examples
#' dt <- loadSpikes(path, triggerfile)
#' spikeISI(dt)
spikeISI <- function(x, ISIonly = FALSE) {
  if (! "ogspiketimes" %in% attr(x, "class")) {
    stop("spikeISI() should only be used on phy output with original spiketimes!")
  }
  if (ISIonly == FALSE) {
    x[["spiketimes"]][, ISI := time - data.table::shift(time, 1), by = cluster]
    print(x[["spiketimes"]])
  } else {
   x[["spiketimes"]][, .(ISI = time - data.table::shift(time, 1)), by = cluster][!is.na(ISI)]
  }
}
