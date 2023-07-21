mergeSpikes <- function(outputs = list(), ids)
{
  if (class(outputs) != "list") {stop("Phyoutputs to be combined should be in a list")}
  if (length(ids) != length(outputs)) {stop("ids (currently ",length(ids), ") should be the same length as outputs (currently ", length(outputs), ")")
    }
  classes <- lapply(outputs, class)
  comp <- classes[[1]]
  res <- all(unlist(lapply(classes, function(x){identical(x, comp)})))
  if (res == FALSE){stop("Phyoutputs to be combined should have identical classes")}
  rm("classes", "comp", "res")

  outputs <- lapply(outputs, copy)

  n <- length(outputs)

  for (i in 1:n) {
    outputs[[i]]$spiketimes[, cluster := paste0(ids[i], "_", cluster)]
    outputs[[i]]$spiketimes[, session := ids[i]]
    outputs[[i]]$info[, cluster_id := paste0(ids[i], "_", cluster_id)]
    if ("triggers" %in% class(outputs[[1]])) {
      outputs[[i]]$triggers[, ses := ids[i]]
    }
  }


  spikelist <- list()
  infolist <- list()
  if ("triggers" %in% class(outputs[[1]])) {
    triggerlist <- list()
  }

  for (i in 1:n) { # Create lists of dts to be added and add them with rbindlist
    spikelist <- append(spikelist, list(outputs[[i]]$spiketimes))
    infolist <- append(infolist, list(outputs[[i]]$info))
    if ("triggers" %in% class(outputs[[1]])) {
      triggerlist <- append(triggerlist, list(outputs[[i]]$triggers))
    }
  }
  spiketimes <- rbindlist(spikelist)
  rm(spikelist)
  tryCatch(info <- rbindlist(infolist),
           error = function(c)
             warning(
               "Column(s): '", c(setdiff(colnames(infolist[[n-1]]), colnames(infolist[[n]])), setdiff(colnames(infolist[[n]]), colnames(infolist[[n-1]]))), "' does not exist in ", ids[n], ".\nMissing values will be set to NA"
             ),
             info <- rbindlist(infolist, fill = TRUE)
           )

  rm(infolist)
  if ("triggers" %in% class(outputs[[1]])) {
    triggers <- rbindlist(triggerlist)
    rm(triggerlist)
  }

  if ("triggers" %in% class(outputs[[1]])) {
  res <- list(
    "spiketimes" = spiketimes,
    "triggers" = triggers,
    "info" = info
  )
  } else {
    res <- list(
      "spiketimes" = spiketimes,
      "info" = info
    )
  }
  attr(res, "class") <- c(attr(outputs[[1]], "class"), "combined")
  attr(res, "sessions") <- ids
  attr(res, "tfactorms") <- attr(outputs[[1]], "tfactorms")
  return(res)
}
