#' Produces a time-distance \code{t-x} trajectory for the lead vehicle, \code{vehicle = 1}.
#'
#' @param lane, a matrix
#' @usage plotlane(lane)
# #' @examples
# #' plane(lane)
#' @export
plotlane <- function(lane) {
  min.    <- min(as.numeric(unlist(lane)), na.rm = TRUE)
  max.    <- max(as.numeric(unlist(lane)), na.rm = TRUE)
  ylim    <- c(min., max.)
  t       <- lane[,1]
  lane    <- lane[,-c(1,2)]
  x       <- lane[,1]
  tstart  <- t[0]
  tend    <- t[length(t)]
  plot(t,x, type = "l", xlab = "t, seconds", ylab = "x, feet", ylim, xlim = c(tstart,tend),col = "orange")
  lane    <- lane[,-c(1:3)]
  x       <- lane[,1]
  lines(t,x, col = "blue")
}
