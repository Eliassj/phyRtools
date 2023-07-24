plotcorrgram <- function(x, clstr, bins = 100, windw = 50, refperiod = 1)
{
  if (!"corrgram" %in% attr(x, "class")){stop("Should only be used on output from corr()")}
  if (windw > attr(x, "windwlim")){stop("Max windw in", x, " is ", attr(x, "windwlim"))}
  swindw <- windw * attr(x, "tfactorms")
  ggplot(x[cluster == clstr & time >= -swindw & time <= swindw])+
    geom_histogram(
      aes(
        x = time / attr(x, "tfactorms")
      ),
      bins = bins,
      fill = standardcol,
      color = NA,
      alpha = .75
    )+
    geom_vline(xintercept = c(refperiod, -refperiod), color = "darkgrey", alpha = .8)+
    scale_y_continuous(expand = c(0,0))+
    labs(
      title = paste0("Cluster ", clstr),
      caption = paste0("Window: +/-", windw, "ms\nRefractory period: ", refperiod, "ms")
    )+
    corrgramtheme
}
