#' Create dt with spikes/period
#'
#' Summarizes the spiketimes to spikes/period.
#' Removes the attribute "\code{ogspiketimes}" and adds the specified \code{period} as an attribute.
#'
#' @param x A list object created by \code{\link{loadSpikes}}.
#' @param period The period in milliseconds by which to summarize.
#' @param full Should a "full" time series including periods without spikes be returned? Defaults to \code{TRUE}.
#'
#' @return A list object with spikes per period as specified by \code{per}. Includes periods with no spikes by default.
#' @export
#'
#' @examples
#' # Get spikes per second
#' dt <- loadSpikes(path, triggerfile)
#' dt <- spikesper(dt, 1)
#' # Get spikes per 10 seconds
#' dt <- spikesper(dt, 10)
spikesper <- function(x, period, full = TRUE) {
  period <- period * 30
  k <- copy(x)
  if ("triggered" %in% attr(k, "class")) {
    b <- c("cluster", "time")
    warning("It is usually better to use rel_response() to get frequencies around triggers.")
    } else {b <- c("cluster", "time")}
  k[["spiketimes"]][, time := round(time / period) * period]
  k[["spiketimes"]] <- k[["spiketimes"]][,
                    n := .N,
                    by = b
                    ][,
                      lapply(.SD, head, 1),
                      by = c("cluster", "time"),
                    ]
  if (full == TRUE) {
    miss <- data.table(cluster = rep(unique(k$spiketimes$cluster), (getmaxtime(k) / period) + 1))
    setorder(miss, cluster)
    miss[, time := seq(0, getmaxtime(k), period), by = cluster]
    miss <- miss[k$info[, .(cluster_id, ch, depth)], on = .(cluster == cluster_id)]
    miss[, n := 0]
    for (c in unique(k$spiketimes$cluster)) {
      miss[cluster == c & !time %in% k$spiketimes[cluster==c, time], keep := TRUE]
    }
    miss <- miss[keep == TRUE, 1:5]
    k$spiketimes <- rbindlist(list(k$spiketimes, miss), fill = TRUE)
    setorder(k$spiketimes, cluster, time)
  }
  k$spiketimes[, frq := n / (period / (attr(k, "tfactorms") * 1000))]
  a <- attr(k, "class")
  attr(k, "class") <- append(a[a != "ogspiketimes"], "summarized")
  attr(k, "period") <- period / 30
  return(k)
}
