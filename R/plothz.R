#' Plot the binned frequency of a cluster
#'
#' The phyoutput used should be the result from [spikesper]
#'
#' @param x A phyoutput from spikesper
#' @param clstrs Clusters to include in plot
#' @param t A numeric vector of size 2 containing desired timespan.
#' @param plotline Should the raw frequency be plotted below the smoothed?
#'
#' @return A plot of smoothed frequency.
#' @export
#'
plothz <- function(x, clstrs, t = "full", plotline = TRUE)
{
  if (!"summarized" %in% attr(x, "class")){stop("spikesper() should be used before plotting frequencies")}
  if (all(t == "full")) {
    t <- c(0, getmaxtime(x) / 30)
  }
  if (plotline == TRUE) {
    p=ggplot(
      x$spiketimes[cluster %in% clstrs & time >= t[1] * 30 & time <= t[2] * 30],
      aes(
        x = time / 30000,
        if (!"triggered" %in% class(x)) {
          y = n * (1 / (attr(x, "period") / 1000))
        } else {
          y = n * (1 / (attr(x, "period") / 1000)) / max(ntrig)
        }

      )
    )+
      geom_line(aes(color = as.factor(cluster)),
                alpha = 1, linewidth = 1)+
      geom_smooth(aes(color = as.factor(cluster)),
                  method = "loess", span = 0.005, se = FALSE, alpha = .5
      )+
      labs(
        x = "Time",
        y = "Hz",
        caption = paste0("Binsize: ", attr(x, "period"), "ms")
      )+
      standarddisc+
      hztheme+facet_grid(rows = vars(depth))
  } else {
    p=ggplot(
      x$spiketimes[cluster %in% clstrs & time >= t[1] * 30 & time <= t[2] * 30],
      aes(
        x = time / 30000,
        y = n * (1 / (attr(x, "period") / 1000))
      )
    )+
      geom_smooth(aes(color = as.factor(cluster)),
                  method = "loess", span = 0.005, se = FALSE, alpha = .5
      )+
      labs(
        x = "Time",
        y = "Hz",
        caption = paste0("Binsize: ", attr(x, "period"), "ms")
      )+
      standarddisc+
      hztheme
  }
  return(p)

}
