#' \code{brktrials3wrapper} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{brktrials3} returns a list of two matrices with 3 times \code{nveh} columns.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param xstart2 start location of the first vehicle in lane 2, (feet), a number
#' @param step size in seconds, a number
#' @param type TRUE to create plots or FALSE otherwise, a logical
#' @param leff vehicle length in feet, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @param usd speed standard deviation, a number
#' @usage brktrials3wrapper(nveh1,nveh2,umn,tstart,tend,xstart1,xstart2,step,type,leff,xfunnel,usd)
#' @examples
#' brktrials3(3, 3, 50.4,  0, 30, -700, -700, 0.25, TRUE,  14, -500, usd)
#' @export
brktrials3wrapper <- function(nveh1, nveh2, umn,  tstart, tend, xstart1, xstart2, step, type, leff, xfunnel, usd) {
  tend.0  <- tend
  tseq    <- seq(tstart,tend,step)
  tlen    <- length(tseq)
  lst     <- brktrials3(nveh1, nveh2, umn,  tstart, tend, xstart1, xstart2, step, type, leff, xfunnel, usd)
  lane1   <- lst[[1]]
  lane2   <- lst[[2]]
  lst     <- enterbottleneck(lane1,lane2,xfunnel,tend,step)
  dfcrit  <- lst[[1]]
  nveh    <- lst[[2]]
  dfcrit1 <- dfcrit[dfcrit[,1] == 1,]
  dfcrit2 <- dfcrit[dfcrit[,1] == 2,]
  ### Lane 1 #############################################################################################################
  zone <- 2
  for(veh in 2:nveh1) {
    lane1.fix  <- fixviolation(veh, zone, lane1, dfcrit1, step, tend.0, leff, browse = TRUE)
    lane1.fix  <- fixdf1df2(veh, lane1.fix, lane1)
    lane1      <- lane1.fix
  }
  if(type == TRUE) {
    tend <- tseq[tlen]
    ylim <- c(-1000,2000)
    par(mfrow = c(1,2), pty = "s")
    plot(tseq, lane1[,2], type = "l", xlab = "t, seconds",
         ylab = "x, feet", ylim, xlim = c(0,tend),col = "orange")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane1[,2]), labels = 1,pos=4,cex=0.5)
    subtitle <- "Desire Lines"
    title(main = "Lane 1", sub = subtitle)
    for(veh in 2:nveh1) {
      df2 <- follower(veh, lane1)
      lines(tseq,df2[,2], col = "blue", lty = 3)
      text(tend, max(df2[,2]), labels = veh, pos=4, cex=0.5)
    }
  }

  ### Lane 2 #############################################################################################################
  zone <- 2
  lane2.fix    <- lane2
  for(veh in 2:nveh2) {
    lane2.fix  <- fixviolation(veh, zone, lane2, dfcrit2, step, tend.0, leff, browse = TRUE)
    lane2.fix  <- fixdf1df2(veh, lane2.fix, lane2)
    lan2       <- lane2.fix
  }
  if(type == TRUE) {
    tend <- tseq[tlen]
    ylim <- c(-1000,2000)
    par(mfrow = c(1,2), pty = "s")
    plot(tseq, lane2[,2], type = "l", xlab = "t, seconds",
         ylab = "x, feet", ylim, xlim = c(0,tend),col = "orange")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane2[,2]), labels = 1,pos=4,cex=0.5)
    subtitle <- "Desire Lines"
    title(main = "Lane 2", sub = subtitle)
    for(veh in 2:nveh2) {
      df2 <- follower(veh, lane2)
      lines(tseq,df2[,2], col = "blue", lty = 3)
      text(tend, max(df2[,2]), labels = veh, pos=4, cex=0.5)
    }
  }
  return(list(lane1,lane2))
}



