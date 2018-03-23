#' Produces a time-distance \code{t-x} trajectoty for \code{vehicle i = 2}.
#'
#' @param L a collection of variables that describe the relationship between a lead \code{l}
#' and a following \code{f} vehicle, a matrix.
#' @param F a collection of variables that describe the relationship between a lead \code{l}
#' and a following \code{f} vehicle, a matrix.
#' @param t4 upper range of the plot, a number.
#' @usage plotfollow(L, F, t4)
plotfollow = function(L, F, t4) {
# the deceleration of the lead vehicle and following vehicle headway affect the x trajectory
# x trajectory spanning t1 to t2 using a xab trajectory.
  t0  <- F[1,1]
  t3  <- F[1,4]
  xf0 <- F[3,1]
  h0  <- F[4,1]
  xl0 <- xf0 + h0
  k0  <- 5280 / h0
  ul0 <- L[2,1]
  uf0 <- F[2,1]
# sight-line
  ul2 <- F[2,3]
  # sightline
  t1  <- F[1,2]
  t2  <- F[1,3]
  xf1 <- F[3,2]
  xl2 <- F[3,3] + F[4,3]
  lines(c(t1, t2), c(xf1, xl2), lty = 2, lwd = 1, col = gray(0.5))
#  points(t2, xl2, pch = 8, cex = 0.5)
#  points(t1, xf1, pch = 8, cex = 0.5)
  text(t0, xf0, "2", pos = 2, cex = 0.5, offset = 0.4)
  # x trajectory from t0 to t1
  tstart <- F[1,1]
  tend   <- F[1,2]
  ustart <- F[2,1]
  uend   <- F[2,2]
  xstart <- F[3,1]
  xend   <- F[3,2]
  lty = 1
  lwd = 1
  col = gray(0)
  trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
  if(t2 < t3) {
# x trajectory spanning t1 to t3 using a xab trajectory.
    tstart <- F[1,2]
    tend   <- F[1,4]
    ustart <- F[2,2]
    uend   <- F[2,4]
    xstart <- F[3,2]
    xend   <- F[3,4]
    lty    <- 1
    lwd    <- 1
    col    <- gray(0)
    trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
    F[1,3] <- F[1,4]

# following vehicle trajectory follows a xab trajectory from t3 to t4
    lines(c(tend, t4), c(xend, xend + uend * (t4 - tend)), col = col)
# Find t5
      t1  <- F[1,4]
      t2  <- t4
      xf1 <- F[3,4]
      xf2 <- xend + uend * (t4 - tend)
      uf1 <- F[2,4]
      uf2 <- uend
      t5  <- findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
  } else {
# x trajectory spanning t1 to t2 using a xab trajectory.
    tstart <- t1
    tend   <- t2
    ustart <- uf1 <- F[2,2]
    uend   <- uf2 <- F[2,3]
    xstart <- xf1 <- F[3,2]
    xend   <- xf2 <- F[3,3]
    lty    <- 1
    lwd    <- 1
    col    <- gray(0)
    trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
    F[1,3] <- F[1,4]
# line between t2 and t4
    lines(c(t2,t4), c(xf2, xf2 + uend * (t4 - t2)), col = col)
# Find t5
    xf1 <- F[3,2]
    xf2 <- F[3,3]
    uf1 <- F[2,2]
    uf2 <- F[2,3]
    t5  <- findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
  }
  # messages
  uf0    <- F[2,1]
  t1     <- round(t1, 2)
  t2     <- round(t2, 2)
  t3     <- round(t3, 2)
  t4     <- round(t4, 2)
  t5     <- round(t5, 2)
  output <- data.frame(vehicle = 1, t1, t2, t3, t4, t5)
  return(output)
}

