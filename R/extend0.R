#' Turn spiketimes into a signal.
#'
#' The signal goes high on times with a spike. 0 = no spike, 1 = spike if not centered. For internal use.
#'
#' Spiketimes should either be in the original 30000Hz or be downsampled to even chunks such as .5ms for example. This should be specified in the \code{by} parameter. Ie if the spikes are sampled at a rate of .5ms (0, 0.5, 1, 1.5...) \code{by} should be .5.
#'
#' @param v A vector of spiketimes.
#' @param by Spacing of observations in the series. Defaults to 1.
#' @param bounds Optional. If no value is provided a signal between the first and last value of v is generated. One value = lower bound, two values = lower and upper bound.
#' @param center Should the time series be centered around 0? Defaults to \code{TRUE}
#' @param vectoronly If \code{TRUE}, returns only the time signal vector. (defaults to \code{FALSE})
#'
#' @return A dt with 2 columns. V1 = time, V2 = signal. If \code{vectoronly = TRUE} returns only the signal (defaults to \code{FALSE}).
#'
extend0 <- function(v, by = 1, bounds = NA, center = TRUE, vectoronly = FALSE)
{
  if (!is.na(bounds)){
    if (length(bounds) > 2) {stop("Bounds should be an integer vector of max length 2")}
    if (!all(bounds == floor(bounds))) {stop("Only integer values allowed as bounds")}
  }
  if (!all(v == floor(v))) {stop("Vector should consist only of integers")}

  if (is.na(bounds)) {
    min <- v[1]
    max <- v[length(v)]
  }
  if (length(bounds) == 1 & !is.na(bounds)) {
    min <- bounds
    max <- v[length(v)]
  }
  if (length(bounds) == 2) {
    min <- bounds[1]
    max <- bounds[2]
  }

  d <- data.table(seq(min, max, by = by),
                  as.integer(0))
  d[V1 %in% v, V2 := 1]
  if (center == TRUE) {
    d[,V2 := V2 - mean(V2)]
  }
  if (vectoronly == FALSE) {return(d)}
  if (vectoronly == TRUE) {return(d$V2)}
}
