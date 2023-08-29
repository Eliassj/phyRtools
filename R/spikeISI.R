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
  if (!"ogspiketimes" %in% attr(x, "class")) {
    stop("spikeISI() should only be used on phy output with original spiketimes!")
  }
  if (ISIonly == FALSE) {
    x[["spiketimes"]][, ISI := time - data.table::shift(time, 1), by = cluster]
    x[["info"]][, ISIlambda := x[["spiketimes"]][!is.na(ISI), lambda(ISI), by = cluster][, V1]]
    x[["info"]][, medianISI := x[["spiketimes"]][!is.na(ISI), median(ISI), by = cluster][, V1]]
    x[["info"]][, meanISI := x[["spiketimes"]][!is.na(ISI), mean(ISI), by = cluster][, V1]]
  } else {
   x[["spiketimes"]][, .(ISI = time - data.table::shift(time, 1)), by = cluster][!is.na(ISI)]
  }
  a <- attr(x, "class")
  attr(x, "class") <- c(a, "ISI")
  return(x)
}
