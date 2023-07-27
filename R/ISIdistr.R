#' Compute distribution of ISI
#'
#' If \code{extend = TRUE} the resulting dt may be *very* large if a too high \code{max} is used.
#'
#' @param x A phyoutput with original spiketimes from the \link{\code{spikeISI}} function.
#' @param binsize Binsize in ms
#' @param relative Should the relative ISI of each cluster be included?
#' @param extend Should "missing" bins (ie those representing 0 ISI) be added?
#' @param minisi The minimum ISI to include when adding "missing" bins.
#' @param maxisi The maximum ISI to include when adding "missing" bins.
#'
#' @return A dt with the distribution of ISI by cluster
#' @export
#'
ISIdistr <- function(x, binsize, extend = TRUE, relative = TRUE, minisi = 0, maxisi = 500)
{
  if (!"phyoutput" %in% attr(x, "class") | !"ISI" %in% attr(x, "class")) {stop("Should only be used on a phyoutput object from the spikeISI() function.")}

  binsize <- binsize * attr(x, "tfactorms")
  isidistr <- x$spiketimes[, floor(ISI / binsize) * binsize, by = cluster]
  isidistr <- isidistr[, .N, by = .(cluster, V1)]
  colnames(isidistr) <- c("cluster", "ISI", "N")
  if (relative) {
    isidistr[, Nrel := N/max(N), by = cluster]
  }
  isidistr <- isidistr[!is.na(ISI)]
  if (extend == TRUE) {
    minisi <- minisi * attr(x, "tfactorms")
    maxisi <- maxisi * attr(x, "tfactorms")
    isidistr <- dtextend(isidistr, col = "ISI", group = "cluster", step = binsize, fill = 0, min = minisi, max = maxisi, ask = FALSE)
  }
  setorder(isidistr, cluster, ISI)
  return(isidistr)
}
