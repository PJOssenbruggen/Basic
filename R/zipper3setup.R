#' \code{zipper3setup} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{zipper3setup} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
#' @param nveh number of vehicles entering the bottleneck, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart start location, (feet), a number
#' @param delt size in seconds, a number
#' @param leff vehicle length in feet, a number
#' @usage zipper3setup(nveh, umn, usd, tstart, tend, xstart, delt, leff)
# #' @examples
# #' zipper3setup(5, 68.4, 4.4, 0, 30, -700, 0.25, 14)
#' @export
#'
zipper3setup <- function(nveh, umn, usd, tstart, tend, xstart, delt, leff) {
  tseq     <- seq(tstart, tend, delt)
  tlen     <- length(tseq)
  y        <- rep(0, tlen)
  tux      <- bmfree2(umn, usd, tstart, tend, xstart, delt)
  tuxy     <- cbind(tux, y)
  for(i in 2:nveh) {
    tux     <- bmfree2(umn, usd, tstart, tend, xstart, delt)
    tux     <- cbind(tux, y)
    u       <- as.numeric(tux[1,2])
    safe.hdwy <- 2*rep(hsafe(u, leff), tlen)
    tux[,3] <- tux[,3] - (i - 1) * safe.hdwy
    tuxy    <- cbind(tuxy, tux[,c(2:4)])
  }
  return(tuxy)
}

