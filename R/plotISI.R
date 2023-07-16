plotISI <- function(x, clstrs, plotlambda = FALSE)
{
  p=ggplot(x$spiketimes[cluster %in% clstrs])+
    geom_histogram(
      aes(
        x = ISI / attr(x, "tfactorms")
      ),
      bins = 100
    )
  return(p+facet_wrap(~cluster, ncol = 2, scales = "free"))
}

#ggplot()+
#  geom_function(data = g$info[cluster_id == 21],
#              fun = function(x){ISIlambda * exp(-x * ISIlambda)},
#              aes(group = cluster_id)
#)
