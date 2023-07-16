corr <- function(x, samples = 10000, windw = 100, clstrs = "all")
{
  if (all(clstrs == "all")) {clstrs <- attr(x, "clusters")}

  windw <- windw * attr(x, "tfactorms")

  o <- data.table(
    "cluster" = clstrs,
    "corr" = as.list(list(NA))
  )

  for (c in clstrs) {

    refvec <- x$spiketimes[cluster == c, time]
    dtsample <- x$spiketimes[cluster == c, .(time)][, .SD[sample(.N, min(samples,.N))]] # Sample spikes from each cluster


    dtsample[, ref := list(list(refvec[refvec >= time - windw & refvec <= time + windw])), by = seq_len(nrow(dtsample))]
    dtsample <- unnest_dt(dtsample, col = ref, id = time)
    colnames(dtsample) <- c("time", "rel")
    dtsample[, rel := rel - time]
    dtsample <- dtsample[!rel == 0]
    o[cluster == c, corr := dtsample$rel]
    message("Correlogram for ", c, " done")
  }
  o <- unnest_dt(o, col = corr, id = cluster)
  colnames(o) <- c("cluster", "time")
  a <- attr(o, "class")
  attr(o, "class") <- append(a, c("phyoutput", "corrgram"))
  attr(o, "clusters") <- clstrs
  attr(o, "tfactorms") <- attr(x, "tfactorms")
  attr(o, "windwlim") <- windw
  return(o)
}
