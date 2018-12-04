#' \code{fixviolation2} produces of a safe headways for a following vehicle
#'
#' @return \code{fixviolation2} is creates following vehicle trajectory that satisfies the safe headway rule.
#' @param veh vehicle, a number
#' @param zone from \code{dfcrit} matrix for vehicle \code{veh}, a number
#' @param df1df2 leading and following vehicle information, a matrix
#' @param dfcrit critical time, a matrix
#' @param delt time step, a number
#' @param tend.0 end time for over the long time range, a number
#' @param leff effective vehicle length in feet, a number
#' @param xfunnel location where the lane drop is located, a number
#' @param browse to inspect \code{fixviolation2} plot or FALSE otherwise
#' @usage fixviolation2(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel, browse)
# #' @examples
# #' fixviolation2(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel, browse)
#' @export
fixviolation2 <- function(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel, browse) {
  # create df1 and df2
  ucol   <- 3*(veh-1) - 2
  xcol   <- 3*(veh-1) - 1
  ycol   <- 3*(veh-1)
  df1    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
  ucol.  <- 3*veh - 2
  xcol.  <- 3*veh - 1
  ycol.  <- 3*veh
  df2    <- cbind(df1df2[,ucol.], df1df2[,xcol.], df1df2[,ycol.])
  t      <- seq(0,tend.0,delt)
  df1    <- cbind(t,df1)
  df2    <- cbind(t,df2)
  tcrit.  <- as.numeric(dfcrit[veh,4])
  xcrit1. <- df1[df1[,1]== tcrit.,3]
  xcrit2. <- df2[df2[,1]== tcrit.,3]
  # if(TRUE) forces a graph to be drawn
  if(browse == TRUE) {
    par(mfrow = c(1,1), pty = "s")
    plot(df1[,1], df1[,3], typ = "l", ylim = c(-1200,3500), ylab = "x", xlab = "t")
    abline(h = c(0,xfunnel), col = gray(0.8))
    abline(v = c(0), col = gray(0.8))
    lines(df1[,1],df1[,3],lty=1,col="yellow",lwd=2.5)
    lines(df1[,1],df1[,3],lty=2,col="red",  lwd=0.5)
    lines(df2[,1],df2[,3],lty=1,col="blue",lwd=0.5)
    text(tend, df1[df1[,1] == tend,3], labels = veh-1, pos = 4, cex = 0.5)
    text(tend, df2[df2[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
    title(main = "Fix violation")
    axis(side = 3, labels = veh, at = 0)
    points(as.numeric(dfcrit[veh,3]), xfunnel, pch = 16, col = "red", cex = 0.75)
    points(as.numeric(dfcrit[veh,4]), 0, pch = 1, col = "black", cex = 0.75)
  }
  leadveh <- veh - 1
  if(browse == TRUE) {
    for(v in 1:leadveh) {
      if(v == 1) {
        ucol   <- 1
        xcol   <- 2
        ycol   <- 3
        df1.    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
        df1.    <- cbind(t,df1.)
        lines(df1.[,1],df1.[,3], lty = 1, col= gray(0.5))
        text(tend, df1.[df1.[,1] == tend,3], labels = v, pos = 4, cex = 0.5)
      } else {
        ucol   <- 3*v - 2
        xcol   <- 3*v - 1
        ycol   <- 3*v
        df1.    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
        df1.    <- cbind(t,df1.)
        lines(df1.[,1],df1.[,3], lty = 1, col= gray(0.5))
        text(tend, df1.[df1.[,1] == tend,3], labels = v, pos = 4, cex = 0.5)
      }
    }
  }
  df2.fix  <- df2
  tstart   <- as.numeric(dfcrit[veh,3])
  X        <- findviolation(tstart, tend.0, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  tcrit    <- findviolation(tstart, tend.0, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
  if(sum(X) == 0) {
    if(browse == TRUE) {
      tseq <- seq(tstart,tend,delt)
      lines(df2.fix[,1],df2.fix[,3], col = "black", lwd = 2)
      lines(df2.fix[,1],df2.fix[,3], col = "green", lwd = 3, lty = 2)
      text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
    }
    browser()
    return(df2.fix[,-1])
  }
  # Zone 2
  tstart   <- as.numeric(dfcrit[veh,3])
  tend     <- as.numeric(dfcrit[veh,4])
  X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
  if(browse == TRUE) {
    lines(df1[,1],df1[,3], lty = 1, col="yellow")
    lines(df1[,1],df1[,3], lty = 2, col="red")
    lines(df2.fix[,1],df2.fix[,3], lty = 3, col = "blue")
    text(tend, df1[df1[,1] == tend,3], labels = veh-1, pos = 4, cex = 0.5)
#    text(tend, df2[df2[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
  }
  # Zone 2
  fraction      <- seq(1,0,-0.01)
  tseq          <- seq(tstart,tend,delt)
  # Zone 2
  tcrit   <- as.numeric(dfcrit[veh,4])
  ustart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
  xstart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
  uend    <- as.numeric(df1[df1[,1] == tend,2])
  xlen    <- length(X)
  tend    <- tcrit
  xend    <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
  space   <- as.numeric(df1[df1[,1] == tend,3]) - xend
  if(space >= hsafe(uend,leff)) xend <- xend else {
    as.numeric(df1[df1[,1] == tend,3]) - hsafe(uend,leff)
  }

  #print(data.frame("Zone 2", veh, tstart, tend, ustart, uend, xstart, xend))
  if(browse == TRUE) {
    points(tstart,xstart, cex = 1.75)
    points(tend,xend, cex = 1.75)
  }
  ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  a       <- ab[1]
  b       <- ab[2]
  tseq    <- seq(tstart,tend,delt)
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
  X       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  ustart  <- u.fix[1]
  xstart  <- x.fix[1]
  uend    <- u.fix[length(tseq)]
  xend    <- x.fix[length(tseq)]
  if(FALSE) {
    tseq  <- seq(tstart,tend,delt)
    points(tstart,xstart, cex = 1, pch = 16)
    points(tend,xend, cex = 2, pch = 1, col = "red")
    lines(tseq,x.fix, col = "tan", lwd = 2)
    text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5, col = gray(0.7))
  }
  df.12  <- data.frame("Zone 2",tstart, tend, ustart, uend, xstart, xend, u.min = min(u.fix),tcrit)
#  print(df.12)
  # Zone 3
  tstart       <- tend
  tend         <- as.numeric(dfcrit[veh,5])
  X            <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  tcrit        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
  df.23        <- data.frame("Zone 3", tstart, tend, ustart, uend, xstart, xend, tcrit)
  #print(X)
  #print(df.23)
  if(sum(X) == 0) {
    # linear x trajectory
    if(FALSE) {
      t4seq    <- seq(tstart,tend.0,delt)
      x.fix.3  <- rep(NA, length(t4seq))
      for(k in 1:length(t4seq)) x.fix.3[k] <- xend + uend * (t4seq[k] - tstart)
      u.fix.3  <- rep(uend, length(t4seq))
      df.3     <- data.frame(x.fix.3,df2.fix = df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend.0,3])
      hdwy.3   <- df.3[,1] - df.3[,2]
      df.3     <- cbind(t = t4seq, df.3, hdwy.3)[-1,]
      t4seq    <- t4seq[-1]
      t4       <- rep(NA, length(t4seq))
      for(i in 1:length(t4seq)) if(df.3[i,4] >= 0) t4[i] <- df.3[i,1]
      t4       <- c(t4,tend.0)
      t4       <- min(t4, na.rm = TRUE)
      t4seq    <- seq(tstart,t4,delt)
      u.fix.3  <- rep(uend, length(t4seq))
      x.fix.3  <- rep(NA, length(t4seq))
      for(k in 1:length(t4seq)) x.fix.3[k] <- xend + uend * (t4seq[k] - tstart)
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= t4,2] <- u.fix.3
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= t4,3] <- x.fix.3
    }
    if(FALSE) {
      lines(t4seq,x.fix.3, col = "black", lwd = 2)
      lines(df2.fix[,1],df2.fix[,3])
    }
  } else {
    # Zone 3
    for(j in 1:length(fraction))  {
      if(j == 1) {
        ustart  <- uend
        xstart  <- xend
        uend    <- as.numeric(df1[df1[,1] == tend,2])
        xend    <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
        #print(data.frame(j, tstart, tend, xstart, xend))
      } else {
        xend  <- xend * fraction[j]
        #print(data.frame(j, tstart, tend, xstart, xend))
      }
      xend1   <- as.numeric(df1[df1[,1] == tend,3])
      headway <- xend1 - xend
      if(headway < hsafe(uend, leff)) xend <- xend - hsafe(uend, leff)
      # part 1
      X       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
      tcrit   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
      #print(data.frame("Zone 3. Part 1",browse,j, sumX = sum(X[-2])))
      #print(data.frame(j, tstart, tend, xstart, xend))
      if(sum(X) <= 1) {
        if(browse == TRUE) {
          tseq <- seq(tstart,tend,delt)
          lines(df2.fix[,1],df2.fix[,3], col = "black", lwd = 2)
          text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
        }
        return(df2.fix[,-1])
        break
      }
      tstart. <- tstart
      tend.   <- tcrit
      ustart. <- ustart
      uend.   <- as.numeric(df1[df1[,1] == tcrit,2])
      xstart. <- xstart
      xend1.  <- as.numeric(df1[df1[,1] == tcrit,3]) - hsafe(uend., leff)
      xend2.  <- as.numeric(df2.fix[df2.fix[,1] == tcrit,3]) - hsafe(uend., leff)
      if(abs(xstart. - xend1.) < abs(xstart. - xend2.))  xend. <- xend1. else xend. <- xend2.
      if(tstart. == tend.) break
      ab      <- xabparam(tstart., tend., ustart., uend., xstart., xend.)
      a       <- ab[1]
      b       <- ab[2]
      tseq.   <- seq(tstart.,tend.,delt)
      t0      <- tstart.
      x.fix.  <- xab(xstart.,ustart.,a,b,t = tseq.,t0)
      u.fix.  <- uab(ustart.,a,b,t = tseq.,t0)
      # part 2
      tstart.. <- tend.
      tend..   <- tend
      ustart.. <- uend.
      xstart.. <- xend.
      xend...  <- as.numeric(df1[df1[,1] == tend.0,3])
      xend..   <- as.numeric(df2.fix[df2.fix[,1] == tend.0,3])
      if(xend... < xend..) {
        uend..   <- as.numeric(df1[df1[,1] == tend.0,2])
        xend..   <- xend... - hsafe(uend..,leff)
      } else {
        uend..   <- as.numeric(df2.fix[df2.fix[,1] == tend.0,2])
        xend..   <- xend.. - hsafe(uend..,leff)
      }
      if(tstart.. < tend..) {
        ab      <- xabparam(tstart.., tend.., ustart.., uend.., xstart.., xend..)
        a       <- ab[1]
        b       <- ab[2]
        tseq..  <- seq(tstart..,tend..,delt)
        t0      <- tstart..
        x.fix.. <- xab(xstart..,ustart..,a,b,t = tseq..,t0)
        u.fix.. <- uab(ustart..,a,b,t = tseq..,t0)
        u.fix   <- c(u.fix.,u.fix..[-1])
        x.fix   <- c(x.fix.,x.fix..[-1])
      } else {
        u.fix   <- u.fix.
        x.fix   <- x.fix.
      }
      # Zone 3
      if(min(u.fix) < 0) {
        tseq <- seq(tstart,tend,delt)
        for(m in 1:length(tseq)) {
          u.lin    <- (xend - xstart)/(tend - tstart)
          if(u.lin <= 0) u.lin <- 0
          x.fix[m] <- xstart + u.lin * (tseq[m] - tstart)
          u.fix[m] <- u.lin
        }
      }
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
      if(TRUE) {
        tseq <- seq(tstart,tend,delt)
        abline(v = tcrit, lty = 3)
        lines(tseq,x.fix, col = "pink", lwd = 2)
        text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5, col = gray(0.7))
      }
    }
  }
  # Fix Zone 1 Spill Back t1 to t2
  t1    <- as.numeric(dfcrit[veh,3])
  t2    <- as.numeric(dfcrit[veh,4])
  t3    <- max(as.numeric(df2.fix[df2.fix[,3] <= 0,1]))
  u1    <- as.numeric(df2.fix[df2.fix[,1] == t1,2])
  u2    <- as.numeric(df2.fix[df2.fix[,1] == t2,2])
  u3    <- as.numeric(df2.fix[df2.fix[,1] == t3,2])
  #print(data.frame("Fix Spill Back", t1, t2, t3, u1, u2, u3))
  tstart  <- t1
  tend    <- t2
  ustart  <- u1
  u1      <- u2
  xstart  <- as.numeric(df2.fix[df2.fix[,1] == t1,3])
  uend    <- u2
  xend    <- as.numeric(df2.fix[df2.fix[,1] == t2,3])
  x1      <- as.numeric(df2.fix[df2.fix[,1] == t1,3])
  ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  a       <- ab[1]
  b       <- ab[2]
  tseq    <- seq(tstart,tend,delt)
  t0      <- tstart
  x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
  u.fix   <- uab(ustart,a,b,t = tseq,t0)
  if(browse == TRUE) {
    lines(tseq,x.fix, col = "tan", lwd = 3)
    points(tstart,xstart, cex = 0.5, pch = 16)
    points(tend,xend, cex = 0.5, pch = 16)
  }
  df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
  df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
  # t0 to t1
  tstart  <- 0
  tend    <- t1
  ustart  <- as.numeric(df2.fix[1,2])
  xstart  <- as.numeric(df2.fix[1,3])
  uend    <- u1
  xend    <- x1
  ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  a       <- ab[1]
  b       <- ab[2]
  tseq    <- seq(tstart,tend,delt)
  t0      <- tstart
  x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
  u.fix   <- uab(ustart,a,b,t = tseq,t0)
  if(browse == TRUE) {
    lines(tseq,x.fix, col = "tan", lwd = 3)
    points(t1,x1, cex = 1.5, pch = 16)
    points(tend,xend, cex = 0.5, pch = 16)
  }
  df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
  df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
  if(browse == TRUE) {
    tseq <- seq(tstart,tend,delt)
    lines(df2.fix[,1],df2.fix[,3], col = "black", lwd = 2)
    lines(df2.fix[,1],df2.fix[,3], col = "green", lwd = 1, lty = 2)
    text(tend, df2.fix[df2.fix[,1] == tend,3], labels = veh, pos = 4, cex = 0.5)
  }
  browser()
  return(df2.fix[,-1])
}

