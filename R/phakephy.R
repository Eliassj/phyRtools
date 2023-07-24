phakephy <- function(clstrcol, tcol, tfactorms = 30)
{
  spikesdt <- data.table(
    "cluster" = clstrcol,
    "time" = tcol
  )
  info <- data.table("cluster_id" = unique(clstrcol))


  value <- list(
   "spiketimes" = spikesdt,
   "info" = info
  )
  attr(value, "class") <- c("phyoutput", "ogspiketimes")
  attr(value, "clusters") <- unique(value[["spiketimes"]]$cluster)
  attr(value, "tfactorms") <- tfactorms
  return(value)
}
