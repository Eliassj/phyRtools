getmaxtime <- function(x)
{
  if (!"phyoutput" %in% attr(x, "class")) {stop("Should only be used on phyoutput")}
  return(x$spiketimes[, max(time)])
}
