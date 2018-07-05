#' The \code{acceldown} function estimates the relative locations of a vehicle downstream of a bottleneck.
#'
#' @return \code{acceldown} uses a stochastic model to show the location of the vehicle after it merges at a bottleneck.
#' @param tstart start time in seconds, a number
#' @param tend end time, a number
#' @param umn start speed (fps) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location of the vehicle (feet), a number
#' @param step size in seconds, a number
#' @usage acceldown(tstart, tend, umn, usd, xstart,  step)
#' @examples
#' acceldown(8.75, 33.25, 47.59, 16.133, -70.36, 0.25)
#' @export
acceldown  <- function(tstart, tend, umn, usd, xstart, step) {
  tseq  <- seq(tstart, tend, by = step)
  W     <- usd * sqrt(step) * rnorm(length(tseq),0,1)
  # Brownian motion of speed
  u     <- rep(umn, length(tseq)) + usd*rnorm(n = length(tseq), mean = 0, sd = sqrt(step))
  x     <- rep(NA, length(tseq))
  x[1]  <- xstart
  for(i in 2:length(tseq)) x[i]  <- x[i-1] + u[i]* step
  df      <- data.frame(t = tseq, u, x)
  return(df)
}
