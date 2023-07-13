fftspikes <- function(x, clstr, len = 30000, sample.frq = 30000, tpr = NA, pwronly = FALSE)
{
  len <- nextn(len, factors = c(2, 3, 5))
  message("len set to ", len, " (may be changed to one with factors 2, 3 & 5)")
  maxtime <- getmaxtime(x)
  taper <- tpr
  v <- x$spiketimes[cluster == clstr, time]

  v <- extend0(v, bounds = c(0, maxtime), center = FALSE, vectoronly = TRUE)
  min <- 1
  max <- len
  res <- rep(0+0i, len)
  pwr <- rep(0, len)
  dft <- rep(0+0i, len)
  it <- (length(v) - (length(v)) %% len) / len
  for (i in 1:it) {
    vtmp <- v[min:max]
    if (!any(is.na(taper))) {vtmp <- vtmp * taper}
    vtmp <- vtmp - mean(vtmp)
    res <- fft(vtmp)
    pwr <- pwr + (1/len) * Mod(res)
    dft <- dft + res
    print(i)
    min <- min + len
    max <- max + len
  }
  min <- (len / 2)
  max <- len + (len / 2) -1
  it <- (length(v) - (length(v)) %% len) / len
  for (i in 1:(it-1)) {
    vtmp <- v[min:max]
    if (!any(is.na(taper))) {vtmp <- vtmp * taper}
    vtmp <- vtmp - mean(vtmp)
    res <- fft(vtmp)
    pwr <- pwr + (1/len) * Mod(res)
    dft <- dft + res
    print(i)
    min <- min + len
    max <- max + len
  }

  pwr <- pwr / ((it * 2)-1)
  dft <- dft / ((it * 2)-1)
  d <- data.table("hz" = 0:(len-1),
                  "pwr" = pwr,
                  "dft" = dft)
  d[, hz := hz * sample.frq / len]
  d[, pwr := Re(pwr)]

  if (pwronly == FALSE) {
    a <- attr(d, "class")
    attr(d, "class") <- c(a, "phyoutput", "specpwr")
    return(d)
  }
  if (pwronly == TRUE) {
    return(d$pwr)
  }
}


