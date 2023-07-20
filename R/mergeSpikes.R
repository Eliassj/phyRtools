mergeSpikes <- function(outputs = list(), ids)
{
  if (class(outputs) != "list") {stop("Phyoutputs to combine should be in a list")}
  if (length(ids) != length(outputs)) {stop("ids (currently ",length(ids), ") should be the same length as outputs (currently ", length(outputs), ")")
    }
  classes <- lapply(outputs, class)
  comp <- classes[[1]]
  res <- all(unlist(lapply(classes, function(x){identical(x, comp)})))
  if (res == FALSE){stop("Phyoutputs to combine should have identical classes")}
  rm("classes", "comp", "res")

  n <- length(outputs)
  nn <- length(names(outputs[[1]]))

  for (i in 1:n) {
    outputs[[i]]$spiketimes[, cluster := paste0(ids[i], "_", cluster)]
    outputs[[i]]$info[, cluster_id := paste0(ids[i], "_", cluster_id)]
    if ("triggers" %in% class(outputs[[1]])) {
      outputs[[i]]$triggers[, session := ids[i]]
    }
  }

  spiketimes <- data.table(matrix(nrow = 0, ncol = length(colnames(outputs[[1]]$spiketimes))))
  colnames(spiketimes) <- colnames(outputs[[1]]$spiketimes)
  info <- data.table(matrix(nrow = 0, ncol = length(colnames(outputs[[1]]$info))))
  colnames(info) <- colnames(outputs[[1]]$spiketimes)
  if ("triggers" %in% class(outputs[[1]])) {

  }
  for (i in n) # rbindlista ihop allt



}
