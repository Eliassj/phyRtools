plotISI <- function(x, clstrs, plotlambda = FALSE, maxt = 100)
{
  p=ggplot(x$spiketimes[cluster %in% clstrs & ISI <= maxt * attr(x, "tfactorms")])+
    geom_density(
      aes(
        x = ISI / attr(x, "tfactorms")
      ),
      fill = standardcol,
      alpha = .7,
      outline.type = "lower"
    )+
    labs(
      title = "ISI",
      x = "ISI"
    )+
    isitheme
  return(p+facet_wrap(~cluster, ncol = 1))
}

#ggplot()+
#  geom_function(data = g$info[cluster_id == 21],
#              fun = function(x){ISIlambda * exp(-x * ISIlambda)},
#              aes(group = cluster_id)
#)
