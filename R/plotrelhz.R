plotrelhz <- function(x, clstrs = NA, by = c("cluster", "trigger", "depthraster", "depthline"), CS = 0, US = 300, abs = FALSE)
{
  if (by[1] == "cluster") {
    if (all(is.na(clstrs))){stop("Clusters not specified")}
    p=ggplot(x$clustermeans[cluster %in% clstrs])+
        geom_line(
          aes(
            x = time / attr(x, "tfactorms"),
            y = relhz
          ),
          stat = "identity"
        )+
      facet_wrap(~cluster, ncol = 1)+
      relhztheme
  }
  if (by[1] == "depthline") {
    if (!all(is.na(clstrs))){warning("Cluster specification will be ignored when plotting spike rate by depth")}
    p=ggplot(x$depthmeans)+
      geom_line(
        aes(
           x = time / attr(x, "tfactorms"),
           if (abs == FALSE){y = relhz} else {y = hz}
        ),
        linewidth = 1
      )+
      facet_grid(rows = vars(depth),
                 switch = "y")+
      labs(
        x = "Time(ms)"
      )+
      relhztheme+
      theme(
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_text(
          angle = 0
        )
      )
  }
  if (by[1] == "depthraster") {
    p=ggplot(x$depthmeans)+
      geom_raster(
        aes(
          x = time / attr(x, "tfactorms"),
          y = as.factor(depth),
          fill = relhz
        )
      )+
      scale_y_discrete(limits = rev)+
      theme(
        legend.position = "bottom"
      )+
      labs(
        x = "Time(ms)"
      )+
      relhztheme+
      theme(
        legend.position = "top",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )+
      greyscale
  }


  return(p+geom_vline(xintercept = c(CS, US),
                      alpha = .25))
}
