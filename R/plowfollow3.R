#' \code{plotfollow3} adds a new trajectory to an existing graph.
#'
#' @param cflist contains information about the lead vehicle, a list
#' @param vehicle a vehicle identifier, a number
#' @param uf speed (mph) of the following vehicle at time \code{t0}, a number
#' @param kf density (vpm) of the following vehicle at time \code{t0}, a number
#' @param ub breakdown speed (mph) for this trajectory, a number
#' @param leff effective vehicle length, a number.
#' @param t4 upper range of plot, a number
#' @usage plotfollow3(cflist, vehicle, uf, kf, ub, leff, t4)
plotfollow3 = function(cflist, vehicle, uf, kf, ub, leff, t4) {
  uf0    <- uf * 5280 / 3600
  kf0    <- kf / 5280
  hf0    <- 2  / kf0
  ub0    <- ub * 5280 / 3600
#  print(data.frame(uf0, ub0, hf0))
  LF     <- cflist[[1]]
  output <- cflist[[2]]
# Lead vehicle information
  F <- L <- LF[[vehicle]]
  t0 <- 0
# A maintain safe headways at time t0 and t1.
# points(t0, xf0, cex = 0.5)
# Find t1 for following vehicle. First, establish a curve for the lead vehicle using L.
# Be careful. The search is between t1 and t2. The reference point (t1,xl1)
# x trajectory spanning t1 to t2 using a xab trajectory.
#  browser()
  if(L[3,3] > L[3,4]) k = 3 else k = 4
  tstart <- L[1,2]
  tend   <- L[1,k]
  ustart <- L[2,2]
  uend   <- L[2,k]
  xstart <- L[3,2]
  xend   <- L[3,k]
  lty <- 1
  lwd <- 1
  col <- gray(0)
  answer3 <- trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
  tseq <- answer3[2]
  useq <- answer3[3]
# Find t1f
  xl0  <- xstart
  xf0  <- xstart - L[4,2]
  ul0  <- ustart
  uf0  <- uf
  t1   <- tstart
  t2   <- tend
  answer1  <- findt1ab(xl0, xf0, uf0, ul0, tseq, useq, t1, t2)
  tf1  <- answer1[1]
  xl1  <- answer1[2]
  uf1  <- answer1[3]
  xf1  <- xl1 - hsafe(uf1,leff )
#  points(tf1, xf1, cex = 0.5, pch = 8)

# Record this information in the F matrix. Remember the F matrix will become the L matrix for the next vehicle (iteration).
  F[1,2] <- tf1
  F[2,2] <- uf1
  F[4,2] <- hsafe(uf1, leff)
  F[3,2] <- xf1
# curve from t0 to tf1 for following vehicle
  tstart <- t0
  tend   <- tf1
  ustart <- uf0
  uend   <- uf1
  xstart <- L[3,1] - hf0
  xend   <- xf1
  lty    <- 1
  lwd    <- 1
  col    <- gray(0)
  answer4 <- trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
  F[1,1]  <- tstart
  F[2,1]  <- ustart
  F[3,1]  <- xstart
  F[4,1]  <- hf0
  text(t0, F[3,1], vehicle + 1, pos = 2, offset = 0.4, cex = 0.5)
# Notes: Following vehicle headway and speed are obtained from random value inputs.
# Find tf2 for the following vehicle. Assumption: t3 = time when vehicle 1 begins traveling at ub0.
  tstart  <- L[1,2]
  tmid    <- L[1,k]
  tend    <- 10000
  ulstart <- L[2,2]
  xlstart <- L[3,2]
  ulmid   <- L[2,k]
  xlmid   <- L[3,k]
  xfstart <- F[3,2]
  uf      <- F[2,2]
  answer2 <- findt2ab(tstart, tmid, tend, ulstart, ulmid, xlstart, xlmid, xfstart, uf, lty, lwd, col)
  dest    <- answer2[1]
  if(ub == 0 & answer2[2] < L[1,4]) {
    tl2     <- L[1,4]
    xl2     <- L[3,4] - hsafe(ub0, leff)
    ul2     <- ub0
  } else {
    tl2     <- answer2[2]
    xl2     <- answer2[3]
    ul2     <- answer2[4]
  }
#  print(answer2)
#  tl2     <- max(c(answer2[2], t4))
#  xl2     <- min(c(answer2[3], L[3,2] + answer2[4] * (tl2 - L[1,2])))
#  ul2     <- answer2[4]
#  test    <- answer2[5]
# t3 = time when following vehicle obtains speed ub0.
  F[1,4]  <- F[1,3]  <- tl2
  F[2,4]  <- F[2,3]  <- ub0
  F[4,4]  <- F[4,3]  <- hsafe(ub0, leff)
  xf2     <- xl2 - F[4,3]
  F[3,4]  <- F[3,3]  <- xf2
# points(tl2, xf2, pch = 15, cex = 0.5)
# print(data.frame(vehicle = vehicle + 1, dest, test))
# points(F[1,4], F[3,4], pch = 15, cex = 0.5)
# curve from t1 to t3 for the following vehicle
  tstart <- F[1,2]
  tend   <- F[1,3]
  ustart <- F[2,2]
  uend   <- F[2,3]
  xstart <- F[3,2]
  xend   <- F[3,3]
  lty    <- 1
  lwd    <- 1
  col    <- gray(0)
  answer5 <- trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
# line between t2 and t4
  lines(c(tend, t4), c(xend, xend + uend * (t4 - tend)))
# Find t5
 if(L[2,3] == 0 | F[[2,4]] == 0) t5 <- NA else {
   t1  <- F[1,2]
   t2  <- F[1,3]
   xf1 <- F[3,2]
   xf2 <- F[3,3]
   uf1 <- F[2,2]
   uf2 <- F[2,3]
   t1  <- F[1,2]
   t2  <- F[1,3]
   t5 <- findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
 }
# Performance measures
  ul0    <- L[2,1]
  xl0    <- L[3,1]
  t3     <- L[1,4]
  t1     <- round(F[1,2], 2)
  t2     <- round(F[1,3], 2)
  t3     <- round(F[1,4], 2)
  t4     <- round(t4, 2)
  t5     <- round(t5, 2)
  output2 <- as.matrix(data.frame(vehicle = vehicle, t1, t2, t3, t4, t5))
  output <- rbind(output, output2)
  output <- as.matrix(output)
  LF = c(LF, list(F))
  cflist <- list(LF, output)
  return(cflist)
}
