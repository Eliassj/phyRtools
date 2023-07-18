plotrelhz <- function(x, clstrs = NA, by = c("cluster", "trigger", "depth"), CS = 0, US = 300)
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
      facet_wrap(~cluster, ncol = 1)
  }
  if (by[1] == "depth") {
    if (!all(is.na(clstrs))){warning("Cluster specification will be ignored when plotting spike rate by depth")}
    p=ggplot(x$depthmeans)+
      geom_line(
        aes(
           x = time / attr(x, "tfactorms"),
           y = relhz,
        ),
        linewidth = 1
      )+
      geom_vline(xintercept = c(CS, US),
                 alpha = .25)+
      facet_wrap(~depth,
                 scales = "free_y")


    #p=ggplot(x$depthmeans)+
    #  geom_raster(
    #    aes(
    #      x = time / attr(x, "tfactorms"),
    #      y = as.factor(depth),
    #      fill = relhz
    #    )
    #  )+
    #  standardcont
  }
  return(p+relhztheme)
}
