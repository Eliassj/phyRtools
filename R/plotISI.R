plotISI <- function(x, clstrs, plotlambda = FALSE, maxt = 100)
{


  lm <- x$info[cluster_id %in% clstrs, .(cluster_id, ISIlambda, medianISI)]
  colnames(lm) <- c("cluster", "ISIlambda", "medianISI")

  p=ggplot(x$spiketimes[cluster %in% clstrs & ISI <= maxt * attr(x, "tfactorms")])+
    geom_density(
      aes(
        x = ISI / attr(x, "tfactorms")
      ),
      fill = standardcol,
      alpha = .7,
      outline.type = "lower"
    )+
    geom_point(
      data = lm,
      aes(
        x = medianISI / attr(x, "tfactorms"),
        y = 0
      ),
      shape = 17,
      alpha = 1,
      size = 2
    )+
    labs(
      x = "ISI(ms) "
    )+
    isitheme
  return(p+facet_wrap(~cluster, ncol = 1))
}

#ggplot()+
#  geom_function(data = g$info[cluster_id == 21],
#              fun = function(x){ISIlambda * exp(-x * ISIlambda)},
#              aes(group = cluster_id)
#)
