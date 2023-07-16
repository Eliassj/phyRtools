phakephy <- function(clstrcol, tcol, tfactorms = 30)
{
  spikesdt <- data.table(
    "cluster" = clstrcol,
    "time" = tcol
  )
  value <- list(
   "spiketimes" = spikesdt
  )
  attr(value, "class") <- c("data.table", "data.frame", "phyoutput", "ogspiketimes")
  attr(value, "clusters") <- unique(value[["spiketimes"]]$cluster)
  attr(value, "tfactorms") <- tfactorms
  return(value)
}
