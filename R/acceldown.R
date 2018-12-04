#' The \code{acceldown} function estimates the relative locations of a vehicle downstream of a bottleneck.
#'
#' @return \code{acceldown} uses a stochastic model to show the location of the vehicle after it merges at a bottleneck.
#' @param tstart start time in seconds, a number
#' @param tend end time, a number
#' @param umn start speed (fps) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location of the vehicle (feet), a number
#' @param delt size in seconds, a number
#' @usage acceldown(tstart, tend, umn, usd, xstart,  delt)
#' @export
acceldown  <- function(tstart, tend, umn, usd, xstart, delt) {
  tseq  <- seq(tstart, tend, by = delt)
  W     <- usd * sqrt(delt) * rnorm(length(tseq),0,1)
  # Brownian motion of speed
  u     <- rep(umn, length(tseq)) + usd*rnorm(n = length(tseq), mean = 0, sd = sqrt(delt))
  x     <- rep(NA, length(tseq))
  x[1]  <- xstart
  for(i in 2:length(tseq)) x[i]  <- x[i-1] + u[i]* delt
  df      <- data.frame(t = tseq, u, x)
  return(df)
}
