#' \code{brktrials3setup} produces a \code{T.} matrix for \code{nveh} vehicles.
#'
#' @return \code{T.} returns information to plot desire-line trajectories.
#' @param nveh number of vehicles entering the bottleneck, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart start location, (feet), a number
#' @param step size in seconds, a number
#' @param browse is TRUE to inspect plot and FALSE otherwise
#' @param leff vehicle length in feet, a number
#' @usage brktrials3setup(nveh, umn, usd, tstart, tend, xstart, step, browse, leff)
#' @examples
#' brktrials3setup(5, 53.1, 0, 0, 40, -700, 0.125, FALSE, 14)
#' @export
#'
brktrials3setup <- function(nveh, umn, usd, tstart, tend, xstart, step, browse, leff) {
  tseq     <- seq(tstart, tend, step)
  tlen     <- length(tseq)
  y        <- rep(0, tlen)
  tux      <- bmfree2(umn, usd, tstart, tend, xstart, step)
  T.       <- cbind(tux, y)
  for(i in 2:nveh) {
    tux     <- bmfree2(umn, usd, tstart, tend, xstart, step)
    tux     <- cbind(tux, y)
    u       <- as.numeric(tux[1,2])
    safe.hdwy <- rep(hsafe(u, leff), tlen)
    tux[,3] <- tux[,3] - (i - 1) * safe.hdwy
    T.      <- cbind(T., tux[,c(2:4)])
  }
  return(T.)
}

