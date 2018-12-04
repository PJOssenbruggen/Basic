#' \code{zipper3} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{zipper3} returns a list of two matrices with 3 times \code{nveh} columns.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param xstart2 start location of the first vehicle in lane 2, (feet), a number
#' @param delta size in seconds, a number
#' @param leff vehicle length in feet, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @param usd speed standard deviation, a number
#' @usage zipper3(nveh1,nveh2,umn,tstart,tend,xstart1,xstart2,delta,leff,xfunnel,usd)
#' @export
zipper3 <- function(nveh1, nveh2, umn, tstart, tend, xstart1, xstart2, delta, leff, xfunnel, usd) {
  print(data.frame(nveh1, nveh2, umn, tstart, tend, xstart1, xstart2, delta,  leff, xfunnel, usd))
  tseq  <- seq(tstart, tend, delta)
  tlen  <- length(tseq)
  y     <- rep(0, tlen)
  nveh  <- nveh1 + nveh2
  umn   <- as.numeric(umn)
  usd   <- as.numeric(usd)
  print(data.frame("zipper3", umn, usd))
  if(nveh1 > 0) lane1 <- zipper3setup(nveh1, umn, usd, tstart, tend, xstart1, delta, leff) else
    lane1 <- {}
  if(nveh2 > 0) lane2 <- zipper3setup(nveh2, umn, usd, tstart, tend, xstart2, delta, leff) else
    lane2 <- {}
  if(nveh1 == 0 & nveh2 > 0) {
    stop("Let nveh1 be non-zero and nveh2 = 0")
  }
  # Step 1. No vehicle may pass upstream of x < xfunnel for lane 1
  u            <- as.numeric(lane1[1,2])
  safe.hdwy    <- rep(hsafe(u, leff), tlen)
  lane1.dup    <- lane1
  pnts         <- {}
  # Plot range
  min.    <- min(as.numeric(unlist(lane1)), na.rm = TRUE)
  max.    <- max(as.numeric(unlist(lane1)), na.rm = TRUE)
  ylim    <- c(min., max.)
  # STEP 2. Form a tuxv matrix for lane 1
  # tuxv  <- tuxv.fix <- cbind(lane1,lane2[,-1])
  # select a vehicle for the vehicle location and aggressiveness
  if(nveh1 > 0) {
    index1   <- seq(3,1+3*nveh1,3)
    times1   <- rep(NA,length(index1))
    for(i in 1:nveh1) {
      tm         <- as.numeric(lane1[lane1[,index1[i]] <= xfunnel,1])
      times1[i]  <- max(tm)
    }
  }
  if(nveh2 > 0) {
    index2   <- seq(3,1+3*nveh2,3)
    times2   <- rep(NA,length(index2))
    for(i in 1:nveh2) {
      tm         <- as.numeric(lane2[lane2[,index2[i]] <= xfunnel,1])
      times2[i]  <- max(tm)
    }
  }
  ### Lane 1 ###########################################################################
  # Vehicle arrival order
  lane    <- lane1
  nveh    <- nveh1
  times   <- times1
  dft     <- data.frame(t = times, ln = rep(1,length(times)))
  o       <- order(dft[,1])
  dft     <- dft[o,]
  nclm    <- seq(2, nveh*3, 3)
  tseq    <- lane[,1]
  lane    <- lane[,-1]
  if(FALSE) {
    # Plot range ylim
    par(mfrow = c(1,2), pty = "s")
    xlim    <- c(tstart,tend)
    min.    <- min(as.numeric(unlist(lane[,nclm])), na.rm = TRUE)
    max.    <- max(as.numeric(unlist(lane[,nclm])), na.rm = TRUE)
    ylim    <- c(min., max.)
    plot(tseq, lane[,2], type = "l", xlab = "t, seconds", lwd = 2,
         ylab = "x, feet", ylim, xlim = xlim,col="blue")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    axis(side = 3, at = tend/2, "Traffic Noise", tick = FALSE, line = -1)
    mtext(bquote(u[0] == .(umn)), at = 0, cex = 0.75, adj = 0)
    mtext(bquote(sigma[U] == .(usd)), at = tend, cex = 0.75, adj = 1)
    index   <- seq(3,1+3*nveh,3)
    for(i in 1:nveh) {
      text(tend, max(lane[,nclm[i]]), labels = i, pos=4, cex=0.5, offset = 0.2)
      text(0, min(lane[,nclm[i]]), labels = i, pos=2, cex=0.5, offset = 0.2)
    }
    for(i in 2:nveh) lines(tseq, lane[,nclm[i]], col = "blue", lwd = 2)
    title(main = "Lane 1", sub = "Lane unsafe and disorderly.")
    legend("topleft", legend = "", bty = "n")
  }
  lane1 <- lane
  ### Lane 2 ###########################################################################
  # Vehicle arrival order
  lane    <- lane2
  nveh    <- nveh2
  times   <- times2
  dft     <- data.frame(t = times, ln = rep(1,length(times)))
  o       <- order(dft[,1])
  dft     <- dft[o,]
  nclm    <- seq(2, nveh*3, 3)
  tseq    <- lane[,1]
  lane    <- lane[,-1]
  if(FALSE) {
    # Plot range ylim
    min.    <- min(as.numeric(unlist(lane[,nclm])), na.rm = TRUE)
    max.    <- max(as.numeric(unlist(lane[,nclm])), na.rm = TRUE)
    ylim    <- c(min., max.)
    plot(tseq, lane[,2], type = "l", xlab = "t, seconds", ylab = "x, feet", ylim,
         xlim = xlim,col="red", lwd = 2)
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    axis(side = 3, at = tend/2, "Traffic Noise", tick = FALSE, line = -1)
    mtext(bquote(u[0] == .(umn)), at = 0, cex = 0.75, adj = 0)
    mtext(bquote(sigma[U] == .(usd)), at = tend, cex = 0.75, adj = 1)
    index   <- seq(3,1+3*nveh,3)
    for(i in 1:nveh) {
      text(tend, max(lane[,nclm[i]]), labels = i, pos=4, cex=0.5, offset = 0.2)
      text(0, min(lane[,nclm[i]]), labels = i, pos=2, cex=0.5, offset = 0.2)
    }
    for(i in 2:nveh) lines(tseq, lane[,nclm[i]], col = "red", lwd = 2)
    title(main = "Lane 2", sub = "Lane unsafe and disorderly.")
    legend("topleft", legend = "", bty = "n")
  }
  lane2 <- lane
  return(list(lane1,lane2))
  ### zipper3 ########################################################################
}

