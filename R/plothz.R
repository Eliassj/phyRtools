#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plothz <- function(x, clstrs, t = "full")
{
  if (!"summarized" %in% attr(x, "class")){stop("spikesper() should be used before plotting frequencies")}
  if (all(t == "full")) {
    t <- c(0, getmaxtime(x) / 30)
  }
  ggplot(
    x$spiketimes[cluster %in% clstrs & time >= t[1] * 30 & time <= t[2] * 30],
    aes(
     x = time / 30000,
     y = n * (1 / (attr(x, "period") / 1000))
    )
  )+
    geom_line(aes(color = as.factor(cluster)),
              alpha = .5)+
    geom_smooth(aes(color = as.factor(cluster)),
                method = "loess", span = 0.005, se = FALSE)+
    labs(
      x = "Time",
      y = "Hz",
      caption = paste0("Binsize: ", attr(x, "period"), "ms")
    )+
    standarddisc+
    hztheme
}
