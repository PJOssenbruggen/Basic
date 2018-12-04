#' \code{trajectoryab} fits a second-order velocity model and produces a \code{t-x} trajectory
#' for \code{vehicle = i} where \code{i = 3,4, ..., nveh}.
#'
#' @param tstart time, a number
#' @param tend time, a number
#' @param ustart speed, a number
#' @param uend speed, a number
#' @param xstart location, a number
#' @param xend location, a number
#' @param delt line type, a number
#' @usage trajectoryab(tstart, tend, ustart, uend, xstart, xend, delt)
#' @export
trajectoryab <- function(tstart, tend, ustart, uend, xstart, xend, delt) {
  start  <- as.numeric(tstart)
  tend   <- as.numeric(tend)
  ustart <- as.numeric(ustart)
  uend   <- as.numeric(uend)
  xstart <- as.numeric(xstart)
  xend   <- as.numeric(xend)

  ab    <- xabparam(
    tstart = tstart,
    tend   = tend,
    ustart = ustart,
    uend   = uend,
    xstart = xstart,
    xend   = xend)
  a <- ab[1]
  b <- ab[2]
# Warning messages:
#  1: In get(object, envir = currentEnv, inherits = TRUE) :
#    restarting interrupted promise evaluation
  tseq <- seq(tstart, tend, by = delt)
  tlen <- length(tseq)
  xseq <- rep(NA, tlen)
  useq <- rep(NA, tlen)
  for(i in 1:tlen) useq[i] <- uab(u0 = ustart, a, b, tseq[i], t0 = tstart)
  for(i in 1:tlen) xseq[i] <- xab(x0 = xstart, u0 = ustart, a, b, tseq[i], t0 = tstart)
  lines(tseq, xseq, col = "black")
  return(list(ab, tseq, useq, xseq))
}
