#' Apply an FFT to phyoutput
#'
#' @param x A phyouput object
#' @param clstr What cluster to apply fft to
#' @param len Length of each window in the same unit as the timeseries in \code{x}. (Ie if x is in the original 30 000Hz setting len to 30000 will make windows 1 second long)
#' @param sample.frq The frequency at which the time series was sampled
#' @param tpr Taper to apply has to be the same length as windw
#' @param pwronly If \code{FALSE} (default) returns the DFT, PSD and frequencies.
#'
#' @return A dt with raw DFT results from welch's method, PSD and frequency bins.
#' @export
#'
fftspikes <- function(x, clstr, len = 30000, sample.frq = 30000, tpr = NA, pwronly = FALSE)
{
  if (!all(is.na(tpr))) {
    if (length(tpr != len)) {stop("Taper should be the same length as window!")}
  }

  if (all(len == "full")) {
    len <- getmaxtime(x)
    isfull <- TRUE
  } else {isfull <- FALSE}
  len <- nextn(len, factors = c(2, 3, 5))

  message("len set to ", len, " (may be changed to one with factors 2, 3 & 5)")

  if (isfull == TRUE) {
    maxtime <- len
  } else {
    maxtime <- getmaxtime(x)
  }
  taper <- tpr
  v <- x$spiketimes[cluster == clstr, time]

  v <- extend0(v, bounds = c(0, maxtime), center = FALSE, vectoronly = TRUE)
  min <- 1
  max <- len
  res <- rep(0+0i, len)
  pwr <- rep(0, len)
  dft <- rep(0+0i, len)
  it <- (length(v) - (length(v)) %% len) / len
  if (isfull == TRUE) {it <- 1}
  message("FFT set 1 in progress")
  for (i in 1:it) {
    vtmp <- v[min:max]
    if (!any(is.na(taper))) {vtmp <- vtmp * taper}
    vtmp <- vtmp - mean(vtmp)
    res <- fftw::FFT(vtmp)
    pwr <- pwr + (1/len) * Mod(res)
    dft <- dft + res
    min <- min + len
    max <- max + len
  }

  if (isfull == FALSE) {
    min <- (len / 2)
    max <- len + (len / 2) - 1
    it <- (length(v) - (length(v)) %% len) / len
    message("FFT set 2 in progress")
    for (i in 1:(it-1)) {
      vtmp <- v[min:max]
      if (!any(is.na(taper))) {vtmp <- vtmp * taper}
      vtmp <- vtmp - mean(vtmp)
      res <- fftw::FFT(vtmp)
      pwr <- pwr + (1/len) * Mod(res)
      dft <- dft + res
      min <- min + len
      max <- max + len
    }
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
    attr(d, "cluster") <- clstr
    attr(d, "params") <- list(
      samplefrq = sample.frq,
      windowlen = len,
      signallen = length(v)
    )
    return(d)
  }
  if (pwronly == TRUE) {
    return(d$pwr)
  }
}


