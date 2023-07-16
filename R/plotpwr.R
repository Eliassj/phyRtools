#' PSD plot for phyoutput
#'
#'
#' @param x An object created by \code{\link{fftspikes}}.
#' @param hzlim The max frequency to include in the plot.
#'
#' @return A ggplot object showing the power spectral density of a cluster.
#' @export
#'

plotpwr <- function(x, hzlim = 500) {
  if (!"specpwr" %in% attr(x, "class")) {stop("Should only be used on output from fftspikes()")}
  ggplot(
    x[hz > 0 & hz < hzlim],
    aes(
      x = hz,
      y = pwr
    )
  )+
    geom_line(color = standardcol,
              alpha = .8)+
    scale_x_continuous(
      minor_breaks = seq(0, hzlim, 25)
    )+
    labs(
      title = attr(x, "cluster"),
      x = "Hz",
      y = "Spectral power"
    )+
    ffttheme
}
