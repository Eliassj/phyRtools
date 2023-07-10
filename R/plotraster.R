#' Title
#'
#' @param x
#' @param clstr
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
plotraster <- function(x, clstr, timeconv = 30, CS = 0, US = 300)
{
  if (!"triggered" %in% attr(x, "class")) {stop("Please use an object returned by triggers() for plotting rasterplots")}
  trigmax <- max(x$spiketimes$ntrig)
  tmin <- min(x$spiketimes$time) / timeconv
  tmax <- max(x$spiketimes$time) / timeconv
  ggplot(x$spiketimes[cluster == clstr])+
    geom_point(
      aes(
        x = time / timeconv,
        y = ntrig
      ),
      color = standardcol,
      size = .8,
      alpha = 1
    )+
    geom_histogram(
      aes(x = time / timeconv,
          y = after_stat(ncount) * trigmax * 0.3),
      position = position_nudge(y = trigmax+2),
      bins = (tmax - tmin),
      alpha = .7,
      fill = standardcol
    )+
    geom_histogram(
      aes(y = ntrig,
          x = (after_stat(ncount) / max(after_stat(ncount))) * 100),
      position = position_nudge(x = 600),
      bins = trigmax,
      alpha = .7,
      fill = standardcol
    )+
    geom_vline(xintercept = c(CS, US), alpha = .5)+
    geom_hline(yintercept = trigmax + 2, linewidth = .2, alpha = .5)+
    scale_y_continuous(breaks = c(0, trigmax))+
    scale_x_continuous(
      breaks = c(tmin, CS-US, CS, US, tmax),
      labels = c(tmin, CS-US, paste0(CS, "(CS)"), paste0(US, "(US)"), tmax),
      limits = c(tmin, tmax + 100)
    )+
    labs(title = paste("Cluster", clstr),
         x = "Time(ms)",
         y = "Stimulation session")+
    rastertheme
}
