#' The \code{accelpass} function estimates the relative locations of two vehicles where one passes the other.
#'
#' @return \code{accelpass} uses a stochastic model to show the locations of vehicle accelerating to pass
#' another vehicle traveling side-by-side at the speed.
#' @param tend end time for simualted run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a number
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane number, a number
#' @param step size in seconds, a number
#' @usage accelpass(tend, umn, usd, xstart, xfunnel, leff, lane, step)
# #' @examples
# #' accelpass(30, 41, 11, -1000, -500, 14, 1, 2)
#' @export
accelpass  <- function(tend, umn, usd, xstart, xfunnel, leff, lane, step) {
  xdet  <- {}
  ydet  <- {}
  tseq  <- seq(0, tend, by = step)
  usd   <- usd * 5280/3600
  W     <- usd * sqrt(step) * rnorm(length(tseq),0,1)
  xdet  <- xstart + umn *5280/3600 * tseq
  # Brownian motion of speed
  udet  <- rep(umn*5280/3600, length(tseq))
  u     <- udet + W
  x     <- rep(NA, length(tseq))
  x[1]  <- xdet[1]
  for(i in 2:length(x)) x[i]  <- x[i-1] + u[i]* step
  y     <- rep(NA, length(tseq))
  if(lane == 1 | lane == 0) for(i in 1:length(x)) {
      if(x[i] > 0) y[i] = 0
      if(x[i] <= 0 & x[i] >= xfunnel) y[i] = 6/xfunnel * abs(x[i])
      if(x[i] < xfunnel) y[i] = -6
    }
  if(lane == 2) for(i in 1:length(x)) {
        if(x[i] > 0) y[i] = 0
        if(x[i] <= 0 & x[i] >= xfunnel) y[i] = -6/xfunnel * abs(x[i])
        if(x[i] < xfunnel) y[i] = 6
  }
  df      <- data.frame(t = tseq, u = rep(umn*5280/3600, length(tseq)), xdet, W, u, x, y)
  lane    <- rep(lane, dim(df)[1])
  df      <- cbind(df, lane)
  colnames(df) <- c("t", "udet", "xdet", "W", "u", "x", "y", "lane")
  return(df)
}
