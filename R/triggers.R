#' Mark spikes by their nearest trigger
#'
#' A function which creates a new \code{phyoutput} object only containing spikes near a trigger (as specified by \code{back} & \code{forward}).
#'
#' @param x A list object as created by \code{\link{loadSpikes}}
#' @param back The time in ms to look backwards from trigger. Defaults to 500.
#' @param forward The time in ms to look forwards from trigger. Defaults to 600.
#'
#' @return A new phyouput object containing info and spikes near triggers.
#' @export
#'
#' @examples
#' dt <- loadSpikes(path, triggerfile)
#' newdt <- triggers(dt)
triggers <- function(x, back = 500, forward = 600) {
  back <- back * 30
  forward <- forward * 30

  dt <- copy(x)
  l <- dt$triggers[, list(list(which(dt$spiketimes$time <= t + forward & dt$spiketimes$time >= t - back)
                                   )),
                  by = n
                  ]

  dt$spiketimes[, ntrig := as.integer(NA)]
  for (t in l$n) {
    set(dt$spiketimes, i = unlist(l[t]$V1), j = "ntrig", value = t)
  }
  dt$spiketimes <- dt$spiketimes[!is.na(ntrig)]
  dt$spiketimes[, time := time - dt$triggers[n == ntrig, t], by = ntrig]
  dt$triggers <- NULL
  attr(dt, "class") <- c("phyoutput", "triggered")
  return(dt)
}
