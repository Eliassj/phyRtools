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
      x = "Hz",
      y = "Spectral power"
    )+
    ffttheme
}
