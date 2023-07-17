rel_response <- function(x, period = 10)
{
  if (!"triggered" %in% class(x)){stop("Should only be used on ouput from triggers()")}
  k <- copy(x)

  baseline <- k$spiketimes[time < 0, .N, by = c("cluster", "ntrig")]
  baseline[, N := (N / back) * attr(k, "tfactorms") * 1000]

}
