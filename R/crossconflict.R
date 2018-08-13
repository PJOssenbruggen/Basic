#' The function \code{crossconflict} a graph showing driver conflict of two vehicles changing lanes.
#'
#' @usage crossconflict()
#' @examples
#' crossconflict()
#' @export
crossconflict <- function() {
  x <- y <- 0
  plot(x, y, type = "l", xlim = c(-20,20), ylim = c(-800,200), xlab = "",
       ylab = "", axes = FALSE)
  polygon(c(-12,-12,12,12,-12),c(-850,250,250,-850,-850),col = gray(0.9))
  abline(v = 0, col = gray(0.8))
  abline(h = c(0,-500), col = gray(0.8))
  lines(c(12,12), c(-850,250))
  lines(c(-12,-12), c(-850,250))
  lines(c(0,0), c(-850,250), lty = 2)
  axis(side = 1, at = 6, labels = "Lane 2", cex = 0.75)
  axis(side = 1, at = -6, labels = "Lane 1", cex = 0.75)
  axis(side = 2, at = -500, labels = "x = -500")
  axis(side = 2, at = 0, labels = "x = 0")
  text(6,-250, label = "Crash", pos = 3)
  title(main = "Conflict")
  axis(side = 1, at = 0, labels = "Upstream", line = 1, cex = 2)
  xstart <- -850
  xend   <- 250
  u0     <- 67 * 5280/3600
  tend   <- (xend-xstart)/u0
  tseq   <- seq(0,tend,0.1)
  xseq   <- xab(xstart,u0,0,0,tseq,0)
  tlen   <- length(tseq)
  y      <- rep(NA, tlen)
  df     <- data.frame(t = tseq, x = xseq, y)
  for(i in 1: tlen) {
    if(df[i,2] >= 0) df[i,3] <- -6
    if(df[i,2] <= -500) df[i,3] <- 6
    if(-500 < df[i,2] & df[i,2] < 0) df[i,3] <- -12/500 * df[i,2] - 6
  }
  lines(df[,3],df[,2], lwd = 3, col = "blue")
  lines(-df[,3],df[,2], lwd = 3, col = "green")
  points(0, -250, pch = 8, cex = 4, col = "red")
  points(0, -250, pch = 16, cex = 4, col = "red")
}
