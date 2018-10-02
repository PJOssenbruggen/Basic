#' \code{brktrials4wrapper} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
#'
#' @return The \code{brktrials4wrapper}, a wrapper function for \code{bmfree3}, \code{xabparam} and
#' \code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory
#' for the following vehicle, and critical information. The lead vehicle trajectory is not affected. Passing is not permitted.
#' The input matrix \code{lane} contains desire lines for a leading and following vehicle.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed standard deviation, a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param xstart2 start location of the first vehicle in lane 2, (feet), a number
#' @param step time step, a number
#' @param tstart  vehicle crossovers are are permitted below this time, a number
#' @param tend upper time range of simulation, a number
#' @param xfunnel location where the lane drop is located, a number
#' @param leff vehicle length in feet, a number
#' @param type TRUE to create plots or FALSE otherwise, a logical
#' @param browse to inspect \code{fixviolation} to inspect plot or FALSE otherwise
#' @usage brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse)
# #' @examples
# #' brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse)
#' @export
brktrials4wrapper  <- function(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse) {
#### STEP 1. Create lane 1 and 2 datasets ################################################################
  #set.seed(123)
   set.seed(403)
  # set.seed(333)
  ylim <- c(-1000,2200)
  xlim <- c(tstart,tend)
  print(data.frame(nveh1,nveh2,xstart1,xstart2,step,tstart,tend,xfunnel,leff))
  lst   <- brktrials3wrapper(nveh1,nveh2,umn,tstart,tend,xstart1,xstart2,step,type,leff,xfunnel,usd)
  lane1 <- lst[[1]]
  lane2 <- lst[[2]]
  if(type == TRUE) {
    par(mfrow = c(1,2), pty = "s")
    tseq   <- seq(0, tend, step)
    tlen   <- length(tseq)
    tend   <- tseq[tlen]
    tend.0 <- tend
    plot(tseq, lane1[,2], type = "l", xlab = "t, seconds", ylab = "x, feet", ylim=ylim, xlim=xlim, col = "blue")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane1[,2]), labels = 1, pos = 2, cex = 0.5)
    nveh1  <- dim(lane1)[2]/3
    for(veh in 2:nveh1) {
      xcol <- 3*(veh-1) + 2
      lines(tseq,lane2[,xcol], col = "blue")
      text(tend, max(lane1[,xcol]), labels = veh,pos = 2, cex = 0.5)
    }
    title(main = "Lane 1", sub = "Assumption: Zipper Merge")
    plot(tseq, lane2[,2], type = "l", xlab = "t, seconds", ylab = "x, feet", ylim=ylim, xlim=xlim,col = "red")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane2[,2]), labels = 1,pos = 2, cex = 0.5)
    nveh2  <- dim(lane2)[2]/3
    for(veh in 2:nveh2) {
      xcol <- 3*(veh-1) + 2
      lines(tseq,lane2[,xcol], col = "red")
      text(tend, max(lane2[,xcol]), labels = veh, pos = 2, cex = 0.5)
    }
    title(main = "Lane 2", sub = "Assumption: Zipper Merge")
  }
  browser()
#### STEP 2. Order vehicles by the times that they reach x = 0 ####################
  lst    <- enterbottleneck(lane1,lane2,xfunnel,tend,step)
  dfcrit <- lst[[1]]
  nveh   <- lst[[2]]
#### STEP 3. Merge lane 1 and 2 data sets ##########################################
  tend.0 <- tend
  tseq   <- seq(0, tend, step)
  tlen   <- length(tseq)
  df1df2 <- mergedata(lane1,lane2,tlen,dfcrit)

#### STEP 4. Find critical times of df1df2. ########################################
  for(veh in 1:(nveh)) {
    for(zone in 1:3) {
      dfcrit <- zoneviolation(veh, zone, df1df2, dfcrit, step, tend.0, leff)
    }
  }
  # Bottleneck: Zipper Traffic
  if(type == TRUE) {
    par(mfrow = c(1,1), pty = "s")
    tend <- tseq[tlen]
    plot(tseq, df1df2[,2], type = "l", xlab = "t, seconds",ylab = "x, feet", ylim=ylim, xlim=xlim,col = "black")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(df1df2[,2]), labels = 1,pos = 4, cex = 0.5)
    for(veh in 2:nveh) {
      xcol        <- 3*(veh-1) + 2
      lines(tseq,df1df2[,xcol], col = "black", lty = 1)
      text(tend, max(df1df2[,xcol]), labels = veh,pos = 4, cex = 0.5)
    }
    title(main = "Bottleneck: Lane Drop", sub = "Assumption: Zipper Merge")
  }
  print(dfcrit)
  browser()
  # Fix violation for Lane Drop ####################################
    zone <- 2
    for(veh in 2:nveh) {
      df2.fix  <- fixviolation2(veh, zone, df1df2, dfcrit, step, tend.0, leff, TRUE)
      df1df2   <- fixdf1df2(veh, df2.fix, df1df2)
      df2      <- follower(veh, df1df2)
      lines(tseq,df2[,2], lty = 1, col = "yellow", lwd = 3)
      xcol     <- 3*veh - 1
      lines(tseq,df1df2[,xcol], lty = 1, col = "red", lwd = 2)
      text(tend, max(df2[,2]), labels = veh, pos = 4, cex = 0.5)
      ln   <- as.numeric(dfcrit[veh,1])
      vh   <- as.numeric(dfcrit[veh,2])
      lab1 <- paste("(",sep="",ln)
      lab2 <- paste(lab1,sep="",",")
      lab3 <- paste(lab2,sep="",vh)
      lab  <- paste(lab3,sep="",")")
      text(tend, max(as.numeric(df1df2[,xcol])), labels = lab, pos = 2, cex = 0.75)
      browser()
    }

#### STEP 5. Final Results #################################################
  if(type == TRUE) {
    t          <- seq(0,tend.0,step)
    par(mfrow = c(1,1), pty = "s")
    plot(t, df1df2[,3], typ = "n", ylim=ylim, ylab = "x, feet", xlab = "t, seconds")
    abline(h = c(0,-500), col = gray(0.8))
    abline(v = c(0), col = gray(0.8))
    print(dfcrit)
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = -500, labels = expression(x[e]))
    title(main = "Bottleneck: Car Following Effects", sub = "Assumption: Safety Headways")
    legend("topleft", legend = c("Arrival Order:","(Lane, Vehicle)"), cex = 0.75, bty = "n")
    for(veh in 1:nveh) {
      xcol <- 3*veh - 1
      lines(t, as.numeric(df1df2[,xcol]))
      text(tend, max(as.numeric(df1df2[,xcol])), labels = veh, pos = 4, cex = 0.5)
      ln   <- as.numeric(dfcrit[veh,1])
      vh   <- as.numeric(dfcrit[veh,2])
      lab1 <- paste("(",sep="",ln)
      lab2 <- paste(lab1,sep="",",")
      lab3 <- paste(lab2,sep="",vh)
      lab  <- paste(lab3,sep="",")")
      text(tend, max(as.numeric(df1df2[,xcol])), labels = lab, pos = 2, cex = 0.75)
    }
  }
  browser()
#### STEP 6. Capacity Esimation
  capacity(df1df2, tstart, tend, step, xfunnel)
  title(main = "Bottleneck: Performance", sub = "Arrival/Departure Rates")
  return(df1df2)
}



