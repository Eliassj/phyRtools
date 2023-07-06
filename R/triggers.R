#' Mark spikes by their nearest trigger
#'
#' A function which adds a column to \code{x} marking its nearest trigger as specified with the \code{forward} and \code{back} params.
#'
#' @param x A list object as created by \code{\link{loadSpikes}}
#' @param back The time in ms to look backwards from trigger. Defaults to 500.
#' @param forward The time in ms to look forwards from trigger. Defaults to 600.
#'
#' @return The same list object but the "spiketimes" dt has an additional column "ntrig".
#' @export
#'
#' @examples
#' dt <- loadSpikes(path, triggerfile)
#' triggers(dt)
triggers <- function(x, back = 500, forward = 600) {
  back <- back * 30
  forward <- forward * 30
  l <- x[["triggers"]][, list(list(which(x[["spiketimes"]]$time <= t + forward & x[["spiketimes"]]$time >= t - back)
                                   )),
                  by = n
                  ]

  x[["spiketimes"]][, ntrig := as.integer(NA)]
  for (t in l$n) {
    set(x[["spiketimes"]], i = unlist(l[t]$V1), j = "ntrig", value = t)
  }

}
