#' Create dt with spikes/period
#'
#' @param x A list object created by \code{\link{loadSpikes}}.
#' @param period The period in seconds by which to summarize.
#'
#' @return A list object with spikes per period as specified by \code{per}.
#' @export
#'
#' @examples
#' # Get spikes per second
#' dt <- loadSpikes(path, triggerfile)
#' dt <- spikesper(dt, 1)
#' # Get spikes per 10 seconds
#' dt <- spikesper(dt, 10)
spikesper <- function(x, period) {
  period <- period * 30000
  k = x
  per <- k[["spiketimes"]][, round(time / period) * period]
  k[["spiketimes"]] = k[["spiketimes"]][,
                    .(
                      cluster = head(cluster, 1),
                      ch = head(ch, 1),
                      depth = head(depth, 1),
                      n = .N
                    ),
                    by = .(cluster, per)
                    ]
  return(k)
}
