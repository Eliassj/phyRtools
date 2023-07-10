fwhtspikes <- function(x, clstr, len = 1000000, sample.frq = 30000)
{
  len <- nextn(len, factors = c(2, 3, 5))
  maxtime <- getmaxtime(x)
  taper <- gsignal::kaiser(len, 25)
  v <- x$spiketimes[cluster == clstr, time]

  v <- extend0(v, bounds = c(0, maxtime), center = FALSE, vectoronly = TRUE)
  min <- 1
  max <- len
  res <- rep(0+0i, len)
  pwr <- rep(0, len)
  it <- (length(v) - (length(v)) %% len) / len
  for (i in 1:it) {
    vtmp <- v[min:max]
    vtmp <- vtmp * taper
    vtmp <- vtmp - mean(vtmp)
    res <- fft(vtmp)
    pwr <- pwr + (2 * res * Conj(res)) / (sample.frq * len)
    print(i)
    min <- min + len
    max <- max + len
  }
  min <- (len / 2)
  max <- len + (len / 2) -1
  it <- (length(v) - (length(v)) %% len) / len
  for (i in 1:(it-1)) {
    vtmp <- v[min:max]
    vtmp <- vtmp * taper
    vtmp <- vtmp - mean(vtmp)
    res <- fft(vtmp)
    pwr <- pwr + (2 * res * Conj(res)) / (sample.frq * len)
    print(i)
    min <- min + len
    max <- max + len
  }

  pwr <- pwr / ((it * 2)-1)
}


p=ggplot()+
  geom_line(aes(x = 0:999999 * sample.frq / len,
                y = Re(pwr)))+
  scale_y_log10()
ggplotly(p)
