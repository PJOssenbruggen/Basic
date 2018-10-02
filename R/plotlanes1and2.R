#' \code{plotlanes1and2} creates \code{t-x} trajectories for lanes 1 and 2.
#'
#' @param lane1 from \code{brktrials3 <- lst[[1]]}, a matrix
#' @param lane2 from \code{brktrials3 <- lst[[2]]}, a matrix
#' @param tend end time, (seconds), a number
#' @param step size in seconds, a number
#' @param xfunnel, start location of the bottleneck, a number
#' @usage plotlanes1and2(lane1, lane2, tend, step, xfunnel)
#' @export
plotlanes1and2 <- function(lane1, lane2, tend, step, xfunnel) {
  pdf(file = "/Users/PJO/Desktop/Lanes1_2.pdf")
  par(mfrow = c(1,2), pty = "s")
  tstart  <- 0
  nveh1   <- dim(lane1)[2]/3
  nveh2   <- dim(lane2)[2]/3
  min1    <- min(as.numeric(unlist(lane1)), na.rm = TRUE)
  max1    <- max(as.numeric(unlist(lane1)), na.rm = TRUE)
  min2    <- min(as.numeric(unlist(lane2)), na.rm = TRUE)
  max2    <- max(as.numeric(unlist(lane2)), na.rm = TRUE)
  ylim    <- c(min(min1,min2), max(max1,max2))
  tseq    <- seq(tstart, tend, step)
  tend    <- max(tseq)
  x       <- lane1[,2]
  plot(tseq, x, type = "l", xlab = "t, seconds", ylab = "x, feet",
       ylim, xlim = c(tstart,tend),col = "orange")
  title(main = "Lane 1")
  legend("topleft", legend = c("leader", "followers"),
         lty = c(1,1),
         col = c("orange", "blue"),
         bty = "n")
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, xfunnel), col = gray(0.8))
  text(tend, max(x), pos = 1, labels = 1)
  for(veh in 2:nveh1) {
    xcol   <- 3*(veh-1) + 2
    print(data.frame(veh,xcol))
    x      <- lane1[,xcol]
    lines(tseq,x, col = "blue")
    text(tend, max(x), pos = 1, labels = veh)
  }
  x <- lane2[,2]
  plot(tseq, x, type = "l", xlab = "t, seconds", ylab = "x, feet",
       ylim, xlim = c(tstart,tend),col = "orange")
  title(main = "Lane 2")
  legend("topleft", legend = c("leader", "followers"),
         lty = c(1,1),
         col = c("orange", "blue"),
         bty = "n")
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, xfunnel), col = gray(0.8))
  text(tend, max(x), pos = 1, labels = 1)
  for(veh in 2:nveh2) {
    xcol   <- 3*(veh-1) + 2
    x      <- lane2[,xcol]
    lines(tseq,x, col = "blue")
    text(tend, max(x), pos = 1, labels = veh)
  }
  dev.off()
}

