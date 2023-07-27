stability <- function(s, tol)
{
  means <- s$spiketimes[, mean(frq), by = cluster]
  colnames(means) <- c("mcluster", "m")

  out <- s$spiketimes[, ]
}
