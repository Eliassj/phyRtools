#' Calculate relative responses to triggers
#'
#' Responses are summarized by cluster, trigger session and depth. A baseline is calculated as the number of spikes before CS / time included before CS. Each trigger session is given a separate baseline.
#' When a short \code{period} is used a cluster with a low firerate may have bins with higher frequency than true. I.e when a period of 10ms is used, 1 spike in 10 ms translates to 100 spikes/s for that bin. This is usually "fixed" whem averaging across trigger sessions as many bins will be 0.
#'
#'
#' @param x An object produced by \code{triggers()}
#' @param period The period in ms by which to summarize
#' @param depthdiv Should the depths be summarized into equally sized ranges? depthdiv = NA -> don't summarize(default). depthdiv = 4 -> divide into 4 depth ranges and calculate mean at each bin.
#'
#' @return A phyoutput object containing:
#' * \code{spiketimes} - Binned spike frequencies, baseline frequencies by cluster, trigger and depth as well as relative frequency of each bin compared to baseline.
#' * \code{info} - General info about each cluster from phy
#' * \code{clustermeans} - Relative and absolute Hz across trigger sessions by **cluster** and time.
#' * \code{depthmeans} - Relative and absolute Hz across trigger sessions by **depth** and time
#'
#' @export
#'
#' @seealso [triggers()] [plotrelhz()]
rel_response <- function(x, period = 30, depthdiv = NA)
{
  if (!"triggered" %in% class(x)){stop("Should only be used on output from triggers()")}
  k <- copy(x)
  period <- period * attr(k, "tfactorms")
  # Create a baseline fr dt by ntrig and cluster. N = Hz
  clusterbaseline <- k$spiketimes[time < 0, .N, by = c("cluster", "depth", "ntrig")]
  clusterbaseline[, N := N * ((attr(k, "tfactorms") * 1000) / attr(k, "min_t"))]
  colnames(clusterbaseline) <- c("clstr", "dpth", "trig", "basehzcluster")

  depthbaseline <- k$spiketimes[time < 0, .N, by = c("depth", "ntrig")]
  depthbaseline[, N := N * ((attr(k, "tfactorms") * 1000) / attr(k, "min_t"))]
  colnames(depthbaseline) <- c("dpth", "trig", "basehzdepth")

  baseline <- clusterbaseline[depthbaseline, on = .NATURAL]
  rm(depthbaseline, clusterbaseline)

  # Summarise into time periods by cluster, depth & trigger session
  k$spiketimes[, time := floor(time / period) * period]
  k$spiketimes <- k$spiketimes[, .N, by = c("cluster", "ntrig", "time")] #PROBLEM HÄR!
  k$spiketimes[, N := N * attr(k, "tfactorms") * 1000 / period]

  # Add "missing" bins
  mint <- -attr(k, "min_t")
  maxt <- attr(k, "max_t")
  setkey(k$spiketimes, cluster, ntrig)
  k$spiketimes <- k$spiketimes[, .SD[CJ(
                               unique(cluster),
                               unique(ntrig),
                               seq(mint, maxt, by = period)
                               ), on = .(cluster == V1, ntrig == V2, time == V3)]][is.na(N), N := 0]


  # Add info and baseline
  k$spiketimes <- k$spiketimes[k$info[, .(cluster_id, ch, depth)], on = .(cluster == cluster_id)]
  k$spiketimes <- k$spiketimes[baseline, on = .(cluster == clstr, ntrig == trig, depth == dpth)]
  k$spiketimes[, relhzcluster := N / basehzcluster]

  # Calculate relative hz
  k$spiketimes[, relhzdepth := N / basehzdepth]
  k$clustermeans <- k$spiketimes[, .(relhz = mean(relhzcluster),
                                     hz = mean(N)
                                     ),
                                 by = .(
                                   cluster,
                                   time,
                                   depth
                                 )]
  k$depthmeans <- k$spiketimes[, .(relhz = mean(relhzdepth),
                                   hz = mean(N)
                               ),
                               by = .(
                                 depth,
                                 time
                               )]

  if (!all(is.na(depthdiv))) {
    if (length(depthdiv == 1)) {
      divs <- max(k$depthmeans$depth) / depthdiv
      lvls <- divs * 1:depthdiv
      lvls <- paste0(round(lvls - divs), " - ", round(lvls), "µm")
      attr(k, "lvls") <- rev(lvls) # FIXA NIVÅERNA!!!!!!!
      k$depthmeans <- k$depthmeans[, depth := ceiling(depth / divs) * divs]
      k$depthmeans <- k$depthmeans[, .(relhz = mean(relhz), hz = mean(hz)), by = .(time, depth)]
      k$depthmeans[, nodelist := k$depthmeans$depth / divs]
      k$depthmeans[, depth := paste0(round(depth - divs), " - ", round(depth), "µm")]
      k$depthmeans[, depth := ordered(depth, levels = lvls)]
    }
  } else {
    dpths <- unique(k$depthmeans$depth)
    k$depthmeans[, nodelist := which(dpths == depth), by = depth]
    attr(k, "lvls") <- rev(dpths)
  }

  attr(k, "period") <- period
  a <- attr(k, "class")
  attr(k, "class") <- append(a[a != "ogspiketimes"], "summarized")
  return(k)
}
