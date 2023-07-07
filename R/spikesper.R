#' Create dt with spikes/period
#'
#' Summarizes the spiketimes to spikes/period.
#' Removes the attribute "\code{ogspiketimes}" and adds the specified \code{period} as an attribute.
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
  k <- copy(g)
  k[["spiketimes"]][, time := round(time / period) * period]
  k[["spiketimes"]] <- k[["spiketimes"]][,
                    n := .N,
                    by = c("cluster", "time")
                    ][,
                      lapply(.SD, head, 1),
                      by = c("cluster", "time"),
                    ]
  a <- attr(k, "class")
  attr(k, "class") <- append(a[a != "ogspiketimes"], "summarized")
  attr(k, "period") <- period / 30000
  return(k)
}
