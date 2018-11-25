#' \code{zipper2setup} produces a \code{T.} matrix for \code{nveh} vehicles.
#'
#' @return \code{T.} returns information to plot desire-line trajectories.
#' @param nveh number of vehicles entering the bottleneck, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart start location, (feet), a number
#' @param step size in seconds, a number
#' @param browse is TRUE to inspect plot and FALSE otherwise
#' @param leff vehicle length in feet, a number
#' @param kfactor density at time \code{t} = 0, a number
#' @usage zipper2setup(nveh, umn, usd, tstart, tend, xstart, step, browse, leff, kfactor)
#' @examples
#' zipper2setup(nveh1, umn, 0, 0, 40, -700, step, TRUE, leff, kfactor)
#' @export
#'
zipper2setup <- function(nveh, umn, usd, tstart, tend, xstart, step, browse, leff, kfactor) {
  tseq     <- seq(tstart, tend, step)
  tlen     <- length(tseq)
  y        <- rep(0, tlen)
  tux      <- bmfree2(umn, usd, tstart, tend, xstart, step, browse = FALSE)
  T.       <- cbind(tux, y)
  for(i in 2:nveh) {
    tux     <- bmfree2(umn, usd, tstart, tend, xstart, step, browse = FALSE)
    tux     <- cbind(tux, y)
    u       <- as.numeric(tux[1,2])
    safe.hdwy <- rep(hsafe(u, leff), tlen)
    tux[,3] <- tux[,3] - (i - 1) * kfactor * safe.hdwy
    T.      <- cbind(T., tux[,c(2:4)])
  }
#  print(T.[1,seq(2,30/2,3)])
#  print(T.[1,seq(3,30/2,3)])
#  print(T.[321,seq(3,30/2,3)])
#  browser()
  return(T.)
}
