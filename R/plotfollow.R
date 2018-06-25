#' Produces a time-distance \code{t-x} trajectoty for \code{vehicle i = 2}.
#'
#' @param L a collection of variables that describe the relationship between a lead \code{l}
#' and a following \code{f} vehicle, a matrix.
#' @param Fw a collection of variables that describe the relationship between a lead \code{l}
#' and a following \code{f} vehicle, a matrix.
#' @param t4 upper range of the plot, a number.
#' @usage plotfollow(L, Fw, t4)
plotfollow = function(L, Fw, t4) {
# the deceleration of the lead vehicle and following vehicle headway affect the x trajectory
# x trajectory spanning t1 to t2 using a xab trajectory.
  t0  <- Fw[1,1]
  t3  <- Fw[1,4]
  xf0 <- Fw[3,1]
  h0  <- Fw[4,1]
  xl0 <- xf0 + h0
  k0  <- 5280 / h0
  ul0 <- L[2,1]
  uf0 <- Fw[2,1]
# sight-line
  ul2 <- Fw[2,3]
  # sightline
  t1  <- Fw[1,2]
  t2  <- Fw[1,3]
  xf1 <- Fw[3,2]
  xl2 <- Fw[3,3] + Fw[4,3]
  lines(c(t1, t2), c(xf1, xl2), lty = 2, lwd = 1, col = gray(0.5))
#  points(t2, xl2, pch = 8, cex = 0.5)
#  points(t1, xf1, pch = 8, cex = 0.5)
  text(t0, xf0, "2", pos = 2, cex = 0.5, offset = 0.4)
  # x trajectory from t0 to t1
  tstart <- Fw[1,1]
  tend   <- Fw[1,2]
  ustart <- Fw[2,1]
  uend   <- Fw[2,2]
  xstart <- Fw[3,1]
  xend   <- Fw[3,2]
  lty = 1
  lwd = 1
  col = gray(0)
  trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
  if(t2 < t3) {
# x trajectory spanning t1 to t3 using a xab trajectory.
    tstart <- Fw[1,2]
    tend   <- Fw[1,4]
    ustart <- Fw[2,2]
    uend   <- Fw[2,4]
    xstart <- Fw[3,2]
    xend   <- Fw[3,4]
    lty    <- 1
    lwd    <- 1
    col    <- gray(0)
    trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
    Fw[1,3] <- Fw[1,4]

# following vehicle trajectory follows a xab trajectory from t3 to t4
    lines(c(tend, t4), c(xend, xend + uend * (t4 - tend)), col = col)
# Find t5
      t1  <- Fw[1,4]
      t2  <- t4
      xf1 <- Fw[3,4]
      xf2 <- xend + uend * (t4 - tend)
      uf1 <- Fw[2,4]
      uf2 <- uend
      t5  <- findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
  } else {
# x trajectory spanning t1 to t2 using a xab trajectory.
    tstart <- t1
    tend   <- t2
    ustart <- uf1 <- Fw[2,2]
    uend   <- uf2 <- Fw[2,3]
    xstart <- xf1 <- Fw[3,2]
    xend   <- xf2 <- Fw[3,3]
    lty    <- 1
    lwd    <- 1
    col    <- gray(0)
    trajectoryab(tstart, tend, ustart, uend, xstart, xend, lty, lwd, col)
    Fw[1,3] <- Fw[1,4]
# line between t2 and t4
    lines(c(t2,t4), c(xf2, xf2 + uend * (t4 - t2)), col = col)
# Find t5
    xf1 <- Fw[3,2]
    xf2 <- Fw[3,3]
    uf1 <- Fw[2,2]
    uf2 <- Fw[2,3]
    t5  <- findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
  }
  # messages
  uf0    <- Fw[2,1]
  t1     <- round(t1, 2)
  t2     <- round(t2, 2)
  t3     <- round(t3, 2)
  t4     <- round(t4, 2)
  t5     <- round(t5, 2)
  output <- data.frame(vehicle = 1, t1, t2, t3, t4, t5)
  return(output)
}

