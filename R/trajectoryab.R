#' \code{trajectoryab} fits a second-order velocity model and produces a \code{t-x} trajectory
#' for \code{vehicle = i} where \code{i = 3,4, ..., nveh}.
#'
#' @param tstart time, a number
#' @param tend time, a number
#' @param ustart speed, a number
#' @param uend speed, a number
#' @param xstart location, a number
#' @param xend location, a number
#' @param lty line type, a number
#' @param lwd line width, a number
#' @param col line color, a factor
#' @usage trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
#'
trajectoryab <- function(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col) {
  a <- xabparam(tstart, tend, ustart, uend, xstart, xend)[1]
  b <- xabparam(tstart, tend, ustart, uend, xstart, xend)[2]
  tseq <- seq(tstart, tend, by = 0.01)
  tlen <- length(tseq)
  xseq <- rep(NA, tlen)
  useq <- rep(NA, tlen)
  for(i in 1:tlen) useq[i] <- uab(u0 = ustart, a, b, tseq[i], t0 = tstart)
  for(i in 1:tlen) xseq[i] <- xab(x0 = xstart, u0 = ustart, a, b, tseq[i], t0 = tstart)
  lines(tseq, xseq, lty = lty, lwd = lwd, col = col)
  ab   <- as.matrix(data.frame(a,b))
  return(list(ab, tseq, useq))
}
