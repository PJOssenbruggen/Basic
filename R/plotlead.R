#' Produces a time-distance \code{t-x} trajectory for the lead vehicle, \code{vehicle = 1}.
#'
#' @param L, a matrix
#' @param theta a \code{gbm} model parameter, a number
#' @param xlim plot range, a vector
#' @param ylim plot range, a vector
#' @param leff effective vehicle length (feet), a number
#' @usage plotlead(L, theta, leff, xlim, ylim)
plotlead = function(L, theta, leff, xlim, ylim) {
  t0 <- L[1,1]
  t3 <- L[1,4]
  u0 <- L[2,1]
  u3 <- L[2,4]
  x0 <- L[3,1]
  x3 <- L[3,4]
  plot(t3, x3, xlab = "t, seconds", ylab = "x, feet", pch = 16, xlim = xlim, ylim = ylim, cex = 0.5)
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  tseq  <- seq(t0, t3, by = 0.01)
  tlong <- length(tseq)
  xseq  <- rep(NA, tlong)
  for(i in 1:tlong) xseq[i] <- x0 + gbmx(u0, theta, t = tseq[i])
  t4 <- xlim[2]
  abline(v = t4, col = gray(0.8))
  lines(tseq, xseq, col = gray(0))
  lines(c(t3, t4), c(x3, x3 + u3 * (t4 - t3)))
  axis(side = 3, at = c(t0, t4), labels = c(expression(t[0]), expression(t[4])))
  text(t0, x0, "1", pos = 2, offset = 0.4, cex = 0.5)
  text(t3,0, expression("("*t[3]*", 0)"), cex = 0.75, pos = 3, offset = 0.5)
}
