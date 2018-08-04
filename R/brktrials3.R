#' \code{brktrials3} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{brktrials3} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
#' @param nveh number of vehicles entering the bottleneck, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility (mph) for \code{umn}, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart start location, (feet), a number
#' @param step size in seconds, a number
#' @param type TRUE to create plots or FALSE otherwise, a logical
#' @param leff vehicle length in feet, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @usage brktrials3(nveh, umn, usd, tstart, tend, xstart, step, type, leff, xfunnel)
#' @examples
#' brktrials3(4, 68.4, 4.4, 0, 30, -700, 0.25, FALSE, 14, -500)
#' @export
brktrials3 <- function(nveh, umn, usd, tstart, tend, xstart, step, type, leff, xfunnel) {
  tseq  <- seq(tstart, tend, step)
  tlen  <- length(tseq)
  y     <- rep(NA, tlen)
  nveh  <- round(0.5*nveh,0)
  lane1 <- brktrials3setup(nveh, umn, usd, tstart, tend, xstart, step, type, leff)
  lane2 <- brktrials3setup(nveh, umn, usd, tstart, tend, xstart, step, type, leff)
  tuxv  <- tuxv.fix <- cbind(lane1,lane2[,-1])
  # select a vehicle for the vehicle location and aggressiveness
  dforder <- {}
  nveh    <- 2*nveh
  for(veh in 1:nveh) {
    dfij    <- round(vehid(veh, tuxv),2)
    dffun   <- dfij[dfij[,3] <= xfunnel,]
    dffun   <- dffun[dim(dffun)[1],]
    dffun   <- c(veh, dffun)
    dforder <- rbind(dforder, dffun)
  }
  colnames(dforder) <- c("vehicle","t","u", "x", "y")
  rownames(dforder) <- rep("given",nveh)
  o       <- order(dforder[,2])
  dforder <- dforder[o,]
  veh     <- dforder[,1]
  dfij    <- round(vehid(veh[1], tuxv),2)
  min.    <- min(as.numeric(unlist(tuxv)), na.rm = TRUE)
  max.    <- max(as.numeric(unlist(tuxv)), na.rm = TRUE)
  ylim    <- c(min., max.)
  if(type == TRUE) {
    # STEP 1
    plot(dfij[,1], dfij[,3], typ = "l", xlab = "t, seconds", ylab = "x, feet", ylim, xlim = c(tstart,tend))
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    # print(data.frame("STEP 1", i = 1, dforder, nveh))
  }
  for(i in 2:nveh) {
    # Step 2 desire lines
#    if(i == 4) browser()
    vehorder <- as.numeric(dforder[,1])
    dfij     <- vehid(veh[i], tuxv)
    # print(data.frame("STEP 2: desire-lines", VEHICLE = i, vehorder, nveh))
#    if(type == TRUE) lines(dfij[,1], dfij[,3])
  }
  for(i in 1:(nveh-1)) {
    # STEP 3
#    if(i == 4) browser()
    df1     <- vehid(vehorder[i],   tuxv.fix)
    df2     <- vehid(vehorder[i+1], tuxv.fix)
    df2.fix <- merge3(i,df1,df2,leff,step,xfunnel,usd,ylim,FALSE)
    # print(data.frame("STEP 3", VEHICLE = i, vehorder, nveh))
    if(type == TRUE) {
      lines(df1[,1], df1[,3], lwd = 2, lty = 1)
      text(tend, max(df1[,3]), labels = veh[i], pos = 4)
      # print(data.frame("merge3", VEHICLE = i))
      lines(df2.fix[,1], df2.fix[,3], lwd = 2, lty = 1)
      text(tend, max(df2.fix[,3]), labels = veh[i+1], pos = 4)
    }
    df       <- vehid(vehorder[i], tuxv)
    ufix     <- df2.fix[,2]
    xfix     <- df2.fix[,3]
    tuxv.fix <- tuxvfix3(i+1, vehorder, nveh, tuxv.fix, ufix, xfix)
  }
  return(list(tuxv, tuxv.fix, vehorder))
}
