spikemt <- function(x, clstr, nw, k = "auto", sample.frq = 30000)
{
  v <- x$spiketimes[cluster == clstr, time]

  v <- extend0(v, vectoronly = TRUE)

  len <- length(v)

  if (all(k == "auto")) {
    k <- 2 * nw
  }

  tprs <- multitaper::dpss(len, k, nw, returnEigenvalues = FALSE)
}
