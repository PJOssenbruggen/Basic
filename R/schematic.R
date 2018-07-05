#' The function \code{schematic} defines key features of a simple bottleneck
#'
#' @usage schematic()
#' @examples
#' schematic()
#' @export
schematic <- function() {
  x <- y <- 0
  plot(x, y, type = "l", xlim = c(-20,20), ylim = c(-800,200), xlab = "", ylab = "", axes = FALSE)
  polygon(c(-6,-6,6,6,12,12,-12,-12,-6),c(0,250,250,0,-500,-850,-850,-500,0),col = gray(0.9))
  abline(v = 0, col = gray(0.8))
  abline(h = c(0,-500), col = gray(0.8))
  lines(c(6,6), c(0,250))
  lines(c(-6,-6), c(0,250))
  lines(c(0,0), c(0,250), lty = 2)
  lines(c(12,12), c(-800,-500))
  lines(c(-12,-12), c(-800,-500))
  lines(c(6,6), c(-850,-500), lty = 2)
  lines(c(-6,-6), c(-850,-500), lty = 2)
  lines(c(6,0), c(-500,0), lty = 2)
  lines(c(-6,0), c(-500,0), lty = 2)
  lines(c(0,0),c(-850,-500))
  axis(side = 1, at = 6, labels = "Lane 2")
  axis(side = 1, at = -6, labels = "Lane 1")
  axis(side = 2, at = -500, labels = "x = -500")
  axis(side = 2, at = 0, labels = "x = 0")
  text(15,0, label = "Bottleneck", pos = 3)
  text(15,-250, label = "Merge Zone", pos = 3)
  title(main = "Simple Lane-Drop Bottleneck")
  axis(side = 1, at = 0, labels = "Upstream", line = 0.5, cex = 2)
}
