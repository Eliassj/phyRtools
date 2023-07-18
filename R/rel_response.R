rel_response <- function(x, period = 30)
{
  if (!"triggered" %in% class(x)){stop("Should only be used on ouput from triggers()")}
  k <- copy(x)
  period <- period * attr(k, "tfactorms")
  # Create a baseline fr dt by ntrig and cluster. N = Hz
  baseline <- k$spiketimes[time < 0, .N, by = c("cluster", "ntrig")]
  baseline[, N := (N / attr(k, "min_t")) * attr(k, "tfactorms") * 1000]
  colnames(baseline) <- c("clstr", "trig", "basehz")

  # Summarise into time periods by cluster & trigger session
  k$spiketimes[, time := floor(time / period) * period]
  k$spiketimes <- k$spiketimes[, .N, by = c("cluster", "ntrig", "time")]
  k$spiketimes[, N := N * attr(k, "tfactorms") * 1000 / period]
  k$spiketimes <- k$spiketimes[k$info[, .(cluster_id, ch, depth)], on = .(cluster == cluster_id)]
  k$spiketimes <- k$spiketimes[baseline, on = .(cluster == clstr, ntrig == trig)]
  k$spiketimes[, relhz := N / basehz]
  k$clustermeans <- k$spiketimes[, .(relhz = mean(relhz),
                                     hz = mean(N)
                                     ),
                                 by = .(
                                   cluster,
                                   time,
                                   ch,
                                   depth
                                 )]
  k$depthmeans <- k$spiketimes[, .(relhz = mean(relhz),
                                   hz = mean(N)
                               ),
                               by = .(
                                 depth,
                                 time
                               )]

  attr(k, "period") <- period
  a <- attr(k, "class")
  attr(k, "class") <- append(a[a != "ogspiketimes"], "summarized")
  return(k)
}
