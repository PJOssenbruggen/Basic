#' The function \code{speedboxplot} plots the data from a data frame \code{I93weekday}
#'
#' @usage speedboxplot()
speedboxplot <- function() {
### Density-speed. Put in bins.
  graphics::boxplot(u ~ bindex, data = quk, col = gray(0.8), axes = FALSE)
  abline(h = 45, lty = 3)
  abline(v = 9, lty = 3)
  axis(side = 2, at = seq(0,80, by = 20))
  axis(side = 2, at = 40, labels = "u, mi / h", tick = FALSE, line = 2)
  axis(side = 1, at = seq(2,22, by = 2), labels = c(seq(10,100,by=10),125))
  axis(side = 1, at = 11, labels = "k, veh / mi", tick = FALSE, line = 2)
  axis(side = 3, at = 9, labels = expression(k^"*"))
  axis(side = 4, at = 45, labels = expression(u^"*"))
  box()
}
