#' \code{fixviolation} produces of a safe headways for a following vehicle
#'
#' @return \code{fixviolation} is creates following vehicle trajectory that satisfies the safe headway rule.
#' @param veh vehicle, a number
#' @param zone from \code{dfcrit} matrix for vehicle \code{veh}, a number
#' @param df1df2 leading and following vehicle information, a matrix
#' @param dfcrit critical times, a matrix
#' @param step time step, a number
#' @param tend.0 end time for over the long time range, a number
#' @param leff effective vehicle lenght, a number
#' @param browse to inspect \code{fixviolation} plot or FALSE otherwise
#' @usage fixviolation(veh, zone, df1df2, dfcrit, step, tend.0, leff, browse)
# #' @examples
# #' fixviolation(veh, zone, df1df2, dfcrit, step, tend.0, leff, browse)
#' @export
fixviolation <- function(veh, zone, df1df2, dfcrit, step, tend.0, leff, browse) {
  # create df1 and df2
  print(dfcrit)
  print(data.frame(vehicle = veh))
  df.01  <- df.12 <- df.23 <- {}
  ucol   <- 3*(veh-1) - 2
  xcol   <- 3*(veh-1) - 1
  ycol   <- 3*(veh-1)
  df1    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
  ucol.  <- 3*veh - 2
  xcol.  <- 3*veh - 1
  ycol.  <- 3*veh
  df2    <- cbind(df1df2[,ucol.], df1df2[,xcol.], df1df2[,ycol.])
  t      <- seq(0,tend.0,step)
  df1    <- cbind(t,df1)
  df2    <- cbind(t,df2)
  if(browse == TRUE) {
    par(mfrow = c(1,1), pty = "s")
    plot(df1[,1], df1[,3], typ = "l", ylim = c(-1200,2500), ylab = "x", xlab = "t")
    abline(h = c(0,-500), col = gray(0.8))
    abline(v = c(0), col = gray(0.8))
    lines(df1[,1],df1[,3],lty=1,col="yellow",lwd=2.5)
    lines(df1[,1],df1[,3],lty=2,col="red",lwd=.5)
    lines(df2[,1],df2[,3],lty=1,col="black",lwd=0.5)
    text(tend, df1[df1[,1] == tend,3], labels = veh-1, pos = 4, cex = 0.5)
    text(tend, df2[df2[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
    title(main = "Fix violation")
    axis(side = 3, labels = veh, at = 0)
  }
  # Zone 2
  tstart   <- as.numeric(dfcrit[veh,3])
  tend     <- as.numeric(dfcrit[veh,4])
  df2.fix  <- df2
  X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
  tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]

  if(dfcrit[veh,9] == 0) return(df2[,-1])
  if(sum(X) == 0) return(df2[,-1])
  if(sum(X) > 0 & tcrit == tstart) return(df2[,-1])
  if(browse == TRUE) {
    lines(df1[,1],df1[,3], lty = 1, col="yellow")
    lines(df1[,1],df1[,3], lty = 2, col="red")
    lines(df2.fix[,1],df2.fix[,3], lty = 3, col = "blue")
    text(tend, df1[df1[,1] == tend,3], labels = veh-1, pos = 4, cex = 0.5)
    text(tend, df2[df2[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
  }
    # Zone 2
    fraction      <- seq(1,0,-0.001)
    tseq          <- seq(tstart,tend,step)
    # Zone 2
    for(j in 1:length(fraction)) {
      X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
      tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
      ustart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
      xstart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
      if(X[1] == 1) {
        xstart  <- xstart - hsafe(ustart, leff)
      }
      uend    <- as.numeric(df2.fix[df2.fix[,1] == tend,2])
      xlen    <- length(X)
      tend    <- tcrit
      if(X[xlen] == 1) xend <- as.numeric(df2.fix[df2.fix[,1] == tend,3]) - hsafe(uend, leff) else
        xend <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
      if(browse == TRUE) {
        points(tstart,xstart, cex = 0.75)
        points(tend,xend, cex = 0.75)
      }
      ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
      a       <- ab[1]
      b       <- ab[2]
      tseq    <- seq(tstart,tend,step)
      t0      <- tstart
      x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
      u.fix   <- uab(ustart,a,b,t = tseq,t0)
      # linear function
      # Zone 2
      if(min(u.fix) < 0) for(m in 1:length(tseq)) {
        u.lin    <- (xend - xstart)/(tend - tstart)
        if(u.lin <= 0) u.lin <- 0
        x.fix[m] <- xstart + u.lin * (tseq[m] - tstart)
        u.fix[m] <- u.lin
      }
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
      X       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
      tcrit   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
      ustart  <- u.fix[1]
      xstart  <- x.fix[1]
      uend    <- u.fix[length(tseq)]
      xend    <- x.fix[length(tseq)]
      if(browse == TRUE) {
        points(tstart,xstart, cex = 1, pch = 16)
        points(tend,xend, cex = 1, pch = 16)
        lines(tseq,x.fix, col = "tan", lwd = 1)
        lines(df2.fix[,1],df2.fix[,3], col = "tan", lwd = 2)
        text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
      }
      df.12  <- data.frame("Zone 2",tstart, tend, ustart, uend, xstart, xend, u.min = min(u.fix),tcrit)
#      print(df.12)
      if(sum(X) == 0) break
    }
    # Zone 3
    tstart       <- as.numeric(dfcrit[veh,4])
    tend         <- as.numeric(dfcrit[veh,5])
    X            <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
    tcrit        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
    if(sum(X) == 0) {
      # linear x trajectory
      t4seq    <- seq(tstart,tend.0,step)
      x.fix.3  <- rep(NA, length(t4seq))
      for(k in 1:length(t4seq)) x.fix.3[k] <- xend + uend * (t4seq[k] - tstart)
      u.fix.3  <- rep(uend, length(t4seq))
      lines(t4seq,x.fix.3, col = "tan", lwd = 2)
      df.3    <- data.frame(x.fix.3,df2.fix = df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend.0,3])
      hdwy.3  <- df.3[,1] - df.3[,2]
      df.3    <- cbind(t = t4seq, df.3, hdwy.3)[-1,]
      t4seq   <- t4seq[-1]
      t4      <- rep(NA, length(t4seq))
      for(i in 1:length(t4seq)) if(df.3[i,4] >= 0) t4[i] <-df.3[i,1]
      t4     <- c(t4,tend.0)
      t4     <- min(t4, na.rm = TRUE)
      t4seq    <- seq(tstart,t4,step)
      u.fix.3  <- rep(uend, length(t4seq))
      x.fix.3  <- rep(NA, length(t4seq))
      for(k in 1:length(t4seq)) x.fix.3[k] <- xend + uend * (t4seq[k] - tstart)
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= t4,2] <- u.fix.3
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= t4,3] <- x.fix.3
      lines(df2.fix[,1],df2.fix[,3])
      text(tend.0, df2.fix[df2.fix[,1] == tend.0,3], labels = veh, pos = 3)
    } else {
      # Zone 3

      for(j in 1:length(fraction))  {
        ustart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,2]) * fraction[j]
        xstart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
        uend    <- as.numeric(df2.fix[df2.fix[,1] == tend,2])
        xend    <- as.numeric(df2.fix[df2.fix[,1] == tend,3]) - hsafe(uend, leff)
        if(as.numeric(df1[df1[,1] == tend,2]) >= xend) xend <- as.numeric(df1[df1[,1] == tend,2]) - hsafe(uend, leff)
        ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
        a       <- ab[1]
        b       <- ab[2]
        tseq    <- seq(tstart,tend,step)
        t0      <- tstart
        x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
        u.fix   <- uab(ustart,a,b,t = tseq,t0)
        # Zone 3
        if(min(u.fix) < 0) for(m in 1:length(tseq)) {
          u.lin    <- (xend - xstart)/(tend - tstart)
          if(u.lin <= 0) u.lin <- 0
          x.fix[m] <- xstart + u.lin * (tseq[m] - tstart)
          u.fix[m] <- u.lin
        }
        df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
        df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
        X       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
        tcrit   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
        df.23   <- data.frame("Zone 3", tstart, tend, ustart, uend, xstart, xend, u.min = min(u.fix), tcrit)
#        print(df.23)
        if(browse == TRUE) {
          points(tstart,xstart, cex = 1, pch = 16)
          points(tend,xend, cex = 1, pch = 16)
          lines(df2.fix[,1],df2.fix[,3], col = "tan", lwd = 2)
          text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
        }
        if(sum(X) <= 1) break
      }
    }
    # Zone 1
    tstart <- 0
    tend   <- as.numeric(dfcrit[veh,3])
    ustart <- as.numeric(df2.fix[1,2])
    xstart <- as.numeric(df2.fix[1,3])
    uend   <- as.numeric(df2.fix[df2.fix[,1] == tend,2])
    xend   <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
    X      <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
    tcrit  <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
#    print(X)
    if(browse == TRUE) {
      points(tstart,xstart, cex = 0.5, pch = 16)
      points(tend,xend, cex = 0.5, pch = 16)
    }
    for(j in 1:length(fraction)) {
      X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[,1]
      tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], step, leff)[1,6]
#      print(X)
      if(sum(X) == 0) break
      ustart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
      xstart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
      if(X[1] == 1) xstart  <- xstart - hsafe(ustart, leff)
      xstart  <- xstart/fraction[j]
      uend    <- as.numeric(df2.fix[df2.fix[,1] == tcrit,2])
      xlen    <- length(X)
      if(X[xlen] == 1) xend <- as.numeric(df2.fix[df2.fix[,1] == tend,3]) - hsafe(uend, leff)
      if(j > 10) ustart  <- fraction[j] * ustart
#      print(data.frame("Zone 1", veh, tstart, tend, ustart, uend, xstart, xend, j))
      if(browse == TRUE) {
        points(tstart,xstart, cex = 0.75)
        points(tend,xend, cex = 0.75)
      }
      ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
      a       <- ab[1]
      b       <- ab[2]
      tseq    <- seq(tstart,tend,step)
      t0      <- tstart
      x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
      u.fix   <- uab(ustart,a,b,t = tseq,t0)
      # linear function
      if(min(u.fix) < 0) for(m in 1:length(tseq)) {
        u.lin    <- (xend - xstart)/(tend - tstart)
        if(u.lin <= 0) u.lin <- 0
        x.fix[m] <- xstart + u.lin * (tseq[m] - tstart)
        u.fix[m] <- u.lin
      }
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
      if(browse == TRUE) {
        points(tstart,xstart, cex = 1, pch = 16)
        points(tend,xend, cex = 1, pch = 16)
        lines(tseq,x.fix, col = "tan", lwd = 1)
        lines(df2.fix[,1],df2.fix[,3], col = "tan", lwd = 2)
        text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
      }
      df.01  <- data.frame("Zone 1",tstart, tend, ustart, uend, xstart, xend, u.min = min(u.fix),tcrit)
#      print(df.01)
    }
  print(df.01)
  print(df.12)
  print(df.23)
  return(df2.fix[,-1])
}
