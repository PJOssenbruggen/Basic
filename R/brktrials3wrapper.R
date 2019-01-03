#' \code{brktrials3wrapper} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{brktrials3} returns a list of two matrices with 3 times \code{nveh} columns.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed standard deviation, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param xstart2 start location of the first vehicle in lane 2, (feet), a number
#' @param delt time-step size in seconds, a number
#' @param run number, a number
#' @param leff vehicle length in feet, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @param kfactor density at time \code{t} = 0, a number
#' @usage brktrials3wrapper(nveh1,nveh2,umn,usd,tstart,tend,xstart1,xstart2,delt,run,leff,xfunnel,kfactor)
# #' @examples
# #' brktrials3wrapper(5, 5, 53.1, 5, 0, 40, -700, -700, 0.125, TRUE,  14, -500, 4/3)
#' @export
brktrials3wrapper <- function(nveh1,nveh2,umn,usd,tstart,tend,xstart1,xstart2,delt,run,leff,xfunnel,kfactor) {
  tend.0  <- tend
  tseq    <- seq(tstart,tend,delt)
  tlen    <- length(tseq)
  ### Arrival Analyzer: brktrials3 ######################################################################################
  lst     <- brktrials3(nveh1,nveh2,umn,tstart,tend,xstart1,xstart2,delt,leff,xfunnel,usd,kfactor)
  lane1   <- lst[[1]]
  lane2   <- lst[[2]]
  lst     <- enterbottleneck(lane1,lane2, xfunnel, tend, delt)
  dfcrit  <- lst[[1]]
  # Lane 1 and 2 Desire-Line Trajectories ################################################################################
  par(mfrow = c(1,2), pty = "s")
  density <- as.numeric(5280/hsafe(umn*5280/3600,leff)/kfactor)
  density <- round(density,0)
  if(run == 1) {
    pdf(file = "/Users/PJO/Desktop/DesireLines.pdf")
    par(mfrow = c(1,2), pty = "s")
    # Lane 1
    tend <- tseq[tlen]
    xcol <- {}
    for(veh in 1:nveh1) xcol <- c(xcol, 2 + 3 * (veh-1))
    xval <- lane1[,xcol]
    ylim <- c(min(xval), max(xval))
    plot(tseq, lane1[,2], type = "l", xlab = "t, seconds",
         ylab = "x, feet", ylim, xlim = c(0,tend + 2),col = "orange")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    axis(side = 3, at = tend/2, labels = "Desire-Lines", line = -1, tick = FALSE)
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    text(tend, max(lane1[,2]), labels = 1,pos=4,cex=0.5)
    if(xstart1 == xstart2) subtitle <- "Side-by-side merge." else subtitle = "Zipper merge."
    title(main = "Lane 1", sub = subtitle)
    for(veh in 2:nveh1) {
      df2 <- follower(veh, lane1)
      lines(tseq,df2[,2], col = "blue", lty = 3)
      text(tend, max(df2[,2]), labels = veh, pos=4, cex=0.5)
    }
    for(veh in 1:nveh1) {
      xcol <- 2 + 3 * (veh-1)
      lines(tseq,lane1[,xcol], col = "blue", lty = 1, lwd = 2)
      text(tend, max(lane1[,xcol]), labels = veh, pos=4, cex=0.5)
    }
    legend("topleft",
     #      title = "",
           legend = c(
             expression(""),
             bquote(u[0] == .(umn)),
             bquote(sigma[U] == .(usd)),
             bquote(k[0] == .(density))
           ),
           cex = c(0.75,0.75,0.75,0.75))
    tend <- tseq[tlen]
    # Lane 2
    xcol <- {}
    for(veh in 1:nveh2) xcol <- c(xcol, 2 + 3 * (veh-1))
    xval <- lane2[,xcol]
    ylim <- c(min(xval), max(xval))
    plot(tseq, lane2[,2], type = "l", xlab = "t, seconds",
         ylab = "x, feet", ylim, xlim = c(0,tend + 2),col = "orange")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    axis(side = 3, at = tend/2, labels = "Desire-Lines", line = -1, tick = FALSE)
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    text(tend, max(lane2[,2]), labels = 1,pos=4, cex=0.5)
    if(xstart1 == xstart2) subtitle <- "Side-by-side merge." else subtitle <- "Zipper merge."
    title(main = "Lane 2", sub = subtitle)
    for(veh in 2:nveh1) {
      df2 <- follower(veh, lane2)
      lines(tseq,df2[,2], col = "red", lty = 3)
      text(tend, max(df2[,2]), labels = veh, pos=4, cex=0.5)
    }
    for(veh in 1:nveh1) {
      xcol <- 2 + 3 * (veh-1)
      lines(tseq,lane2[,xcol], col = "red", lty = 1, lwd = 2)
      text(tend, max(lane2[,xcol]), labels = veh, pos=4, cex=0.5)
    }
    legend("topleft",
           #      title = "",
           legend = c(
             expression(""),
             bquote(u[0] == .(umn)),
             bquote(sigma[U] == .(usd)),
             bquote(k[0] == .(density))
           ),
           cex = c(0.75,0.75,0.75,0.75))
    dev.off()
  }
  ### Lane 1  and 2 #######################################################################################
  zone <- 2
  dfcrit1 <- dfcrit[as.numeric(dfcrit[,1]) == 1,]
  for(veh in 2:nveh1) {
    lane1.fix  <- fixviolation(veh, zone, lane1, dfcrit1, delt, tend.0, leff, xfunnel)
    lane1.fix  <- fixdf1df2(veh, lane1.fix, lane1)
    lane1      <- lane1.fix
  }
  dfcrit2 <- dfcrit[as.numeric(dfcrit[,1]) == 2,]
  for(veh in 2:nveh2) {
    lane2.fix  <- fixviolation(veh, zone, lane2, dfcrit2, delt, tend.0, leff, xfunnel)
    lane2.fix  <- fixdf1df2(veh, lane2.fix, lane2)
    lane2      <- lane2.fix
  }
  par(mfrow = c(1,2), pty = "s")
  return(list(lane1,lane2))
}



