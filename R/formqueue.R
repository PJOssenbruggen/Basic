#' The \code{formqueue} function is used to explain queuing upstream of a bottleneck.
#'
#' @return \code{formqueue} uses a deterministic model to illustrate an ``idealistic'' situtaion,
#' a so-called a ``formqueue merge.''
#' @param tstart start time, a number
#' @param tend end time, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a number
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle lingth(feet), a number
#' @usage formqueue(tstart, tend, umn, usd, xstart, xfunnel, leff)
# #' @examples
# #' formqueue(0, 10, 41, 11.6, -1000, -500, 14)
#' @import grDevices
#' @import stats
#' @import graphics
formqueue  <- function(tstart, tend, umn, usd, xstart, xfunnel, leff) {
  par(mfrow = c(1,1))
  x1 <- x2 <- {}
  y1 <- y2 <- {}
  step  <- tend/10
  tseq  <- seq(0, 5*tend, by = step)
  usd   <- usd * 5280/3600
  W     <- usd * sqrt(step) * rnorm(length(tseq),0,1)
  x1    <- xstart + umn *5280/3600 * tseq
  for(i in 1:length(x1)) x1[i]    <- x1[i] + W[i]
  y1 <- rep(NA, length(tseq))
  x2 <- x1 - rep(hsafe(umn * 5280/3600, leff), length(tseq))
  for(i in 1:length(x1)) {
    if(x1[i] > 0) y1[i] = 0
    if(x1[i] <= 0 & x1[i] >= xfunnel) y1[i] = 6/xfunnel * abs(x1[i])
    if(x1[i] < xfunnel) y1[i] = -6
  }
  for(i in 1:length(x2)) {
    if(x2[i] > 0) y2[i] = 0
    if(x2[i] <= 0 & x2[i] >= xfunnel) y2[i] = -6/xfunnel * abs(x2[i])
    if(x2[i] < xfunnel) y2[i] = 6
  }
  df <- data.frame(t = tseq, y1, x1, y2, x2)
  plot(y1[x1 > -600 & x1 < 200], x1[x1 > -600 & x1 < 200], pch = 16, cex = 0.75, ylim = c(-600, 200),
       xlim = c(-7,7),      col = "green", axes = FALSE, ylab = "", xlab = "")
  points(y2[x2 > -600 & x2 < 200], x2[x2 > -600 & x2 < 200], pch = 16, cex = 0.75, col = "red")
  text(3,0, expression("x = 0"))
  text(0,-500, expression("x = -500"))
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, -500), col = gray(0.8))
  title("Zipper Merge")
  legend("topleft", legend = c("Vehicle 1", "Vehicle 2"),
         col = c("green", "red"),
         pch = c(16,16),
         bty = "n")
}
