#' Load spikes from Phy output
#'
#' Returns a phyoutput containing every spiketime and associated info marked as "good" in Phy.\cr
#' Associated info includes depth, channel, triggertimes (if specified in \code{triggerfile}), amplitude, and mean firerate
#'
#' @param path Path to phy output
#' @param triggerfile Name of triggerchannel, has to be in the 'path' dir and in .csv form.
#' @param minfr The minimum mean firerate to include
#'
#' @return An list-object with classes \code{phyoutput} and \code{ogspiketimes} containing:
#' * "spiketimes", a dt with spiketimes, their cluster, channel and depth.
#' * "triggers", a vector of triggertimes
#' * "info", a dt with info from phy on each cluster
#'
#' All spike-/trigger times are represented in their original 30 000Hz form.
#' Associated attributes of importance:
#' * \code{class} -> ogspiketimes - denotes original spiketimes without summarizing into bins or similar
#' * \code{clusters}: A chr vector of all clusters in contained
#' * \code{tfactorms}: The factor by which to divide the time column to convert it to ms.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' loadSpikes(path = "C:/Users/Elias/Desktop/phyish/Phyr/Våra", triggerfile = "asd.csv")
loadSpikes <- function(path, triggerfile = NA, minfr = 0) {
  np <- reticulate::import("numpy")
  clusterinfo <- data.table::fread(paste0(path, "\\cluster_info.tsv"))[group == "good"]
  if (minfr != 0) {
    message("Removing following due to firerate < ", minfr)
    print(clusterinfo[fr < minfr, c("cluster_id", "fr")])
    clusterinfo <- clusterinfo[fr >= minfr]
  }
  spikesdt <- data.table::data.table("cluster" = as.integer(as.vector(np$load(paste0(path, "\\spike_clusters.npy")))),
                                     "time" = np$load(paste0(path, "\\spike_times.npy")))
  spikesdt <- spikesdt[cluster %in% clusterinfo$cluster_id]
  spikesdt <- spikesdt[clusterinfo[, .(cluster_id, ch, depth)], on = .(cluster == cluster_id)]
  colnames(spikesdt) <- c("cluster", "time", "ch", "depth")

  if (!is.na(triggerfile)) {
    trigger <- data.table::fread(paste0(path, "\\",triggerfile)) # Read triggerchannel

    trig_index <- data.table::data.table("triggers" = trigger[, which(V1 != 0)]) # Extract index of triggers

    trig_index[, triggershift := triggers - data.table::shift(triggers, n = 1)] # Make all sequential values 1

    triggertimes <- trig_index[triggershift != 1, triggers]# Remove all values not = 1
    triggers <- data.table("t" = triggertimes,
                           "n" = 1:length(triggertimes))
    value <- list(
      "spiketimes" = spikesdt,
      "triggers" = triggers,
      "info" = clusterinfo
    )
    attr(value, "class") <- c("phyoutput", "ogspiketimes", "triggers")
    attr(value, "clusters") <- unique(value[["spiketimes"]]$cluster)
    attr(value, "tfactorms") <- 30
  } else {
    value <- list(
      "spiketimes" = spikesdt,
      "info" = clusterinfo,

    )
    attr(value, "class") <- c("phyoutput", "ogspiketimes")
  }
  return(value)
}
