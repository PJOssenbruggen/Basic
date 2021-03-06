#' \code{fixviolation} produces of a safe headways for a following vehicle
#'
#' @return \code{fixviolation} is creates following vehicle trajectory that satisfies the safe headway rule.
#' @param veh vehicle, a number
#' @param zone from \code{dfcrit} matrix for vehicle \code{veh}, a number
#' @param df1df2 leading and following vehicle information, a matrix
#' @param dfcrit critical times, a matrix
#' @param delt time-step, a number
#' @param tend.0 end time for over the long time range, a number
#' @param leff effective vehicle lenght, a number
#' @param xfunnel location where the lane drop is located, a number
#' @usage fixviolation(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel)
# #' @examples
# #' fixviolation(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel)
#' @export
fixviolation <- function(veh, zone, df1df2, dfcrit, delt, tend.0, leff, xfunnel) {
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
  leadveh <- veh - 1
  for(v in 1:leadveh) {
    if(v == 1) {
      ucol   <- 1
      xcol   <- 2
      ycol   <- 3
      df1.    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
      df1.    <- cbind(t,df1.)
    } else {
      ucol   <- 3*v - 2
      xcol   <- 3*v - 1
      ycol   <- 3*v
      df1.    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
      df1.    <- cbind(t,df1.)
    }
  }
  # Zone 2 #############################################################################
  df2.fix  <- df2
  zone.df  <- NA
  tstart   <- as.numeric(dfcrit[veh,4])
  tend     <- as.numeric(dfcrit[veh,5])
  X3       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  tcrit3   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
  tstart   <- as.numeric(dfcrit[veh,3])
  tend     <- as.numeric(dfcrit[veh,4])
  X2       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
  tcrit2   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
  crittable <- data.frame(t1 = as.numeric(dfcrit[veh,3]), tcrit2, sum.X2 = sum(X2), tcrit3, sum.X3 = sum(X3))
  if(sum(X2) == 0 & sum(X3) == 0) return(df2.fix[,-1])
  # Zone 2 df2.fix
  if(sum(X2) > 0) {
    fraction      <- seq(1,0,-0.001)
    tseq          <- seq(tstart,tend,delt)
    for(j in 1:length(fraction)) {
      X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
      tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
      sum.X    <- sum(X)
      if(j == 1) {
        ustart   <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
        xstart   <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
        if(X[1] == 1) xstart  <- xstart - hsafe(ustart, leff)
        uend     <- as.numeric(df2.fix[df2.fix[,1] == tend,2])
        xend     <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
        xlen     <- length(X)
        if(X[xlen] == 1) {
          uend <- as.numeric(df1[df1[,1] == tend,2])
          xend <- as.numeric(df1[df1[,1] == tend,3]) - hsafe(uend, leff)
        }
        if(X[1] == 1) {
          ustart <- as.numeric(df1[df1[,1] == tstart,2])
          xstart <- as.numeric(df1[df1[,1] == tstart,3]) - hsafe(ustart, leff)
        }
      } else {
        ustart <- ustart * fraction[j]
        xend   <- xend - hsafe(ustart, leff)
      }
      ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
      a       <- ab[1]
      b       <- ab[2]
      tseq    <- seq(tstart,tend,delt)
      t0      <- tstart
      x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
      u.fix   <- uab(ustart,a,b,t = tseq,t0)
      # Zone 2
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
      df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
      X       <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
      tcrit   <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
      sum.X.rev  <- sum(X)
      ustart  <- u.fix[1]
      xstart  <- x.fix[1]
      uend    <- u.fix[length(tseq)]
      xend    <- x.fix[length(tseq)]
      df.12   <- data.frame("Zone 2",j,tstart, tend, ustart, uend, xstart, xend)
      if(sum(X) == 0) {
        zone.df <- rbind(zone.df, data.frame(Zone = 2, tstart, tend, ustart, uend, xstart, xend))
        break
      }
      if(sum.X.rev == sum.X) {
        break
      }
    }
  }
  # Zone 3 #################################################################################
  if(sum(X3) >= 0) {
    test    <- NA
    tstart  <- tend
    if(sum(X2) == 0) {
      tstart <- max(df2.fix[df2.fix[,3] <= 0,1])
      ustart <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
      xstart <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
    } else {
      ustart  <- uend
      xstart  <- xend
    }
    tseq    <- seq(0,tend.0,delt)
    tlen    <- length(tseq)
    x2sight <- rep(NA,tlen)
    for(i in 1:tlen) x2sight[i] <- xstart + ustart * (tseq[i] - tstart)
    viol    <- findviolation(tstart, tend.0, tend.0, df1[,-1], df2.fix[,-1], delt, leff)
    df12    <- cbind(df1[,c(1:3)],df2.fix[,c(2,3)],x2sight)
    colnames(df12) <- c("t", "u1", "x1", "u2", "x2","x2sight")
    tseq    <- seq(tstart,tend.0,delt)
    dx2x1   <- df12[,3] - df12[,6]
    df12    <- cbind(df12,dx2x1)
    df12.   <- df12[df12[,1] > tstart,]
    if(tstart == tend.0) {
      tstart <- tend
      test   <- Inf
    } else {
      if(length(df12.[df12.[,7] <= 0,1]) == 0) test <- tend.0 else test <- min(df12.[df12.[,7] <= 0,1], na.rm = TRUE)
      xtest   <- as.numeric(df12[df12[,1] == test,6])
      }
    if(!is.infinite(test)) {
      ustart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,2])
      xstart  <- as.numeric(df2.fix[df2.fix[,1] == tstart,3])
      tend    <- test
      uend    <- min(as.numeric(df1[df1[,1] == tend,2]))
      xend    <- xtest - hsafe(uend,leff)
      uend2   <- as.numeric(df2.fix[df2.fix[,1] == tend,2])
      xend2   <- as.numeric(df2.fix[df2.fix[,1] == tend,3])
      if(tend == tend.0 & xend2 < xend) uend <- uend2
      if(tend == tend.0 & xend2 < xend) xend <- xend2
      if(tstart < tend) {
        ab      <- xabparam(tstart, tend, ustart, uend, xstart, xend)
        a       <- ab[1]
        b       <- ab[2]
        tseq    <- seq(tstart,tend,delt)
        t0      <- tstart
        x.fix   <- xab(xstart,ustart,a,b,t = tseq,t0)
        u.fix   <- uab(ustart,a,b,t = tseq,t0)
        tlen    <- length(tseq)
        if(x.fix[tlen] < xstart | min(u.fix) < 0) {
          for(i in 1:tlen) {
            u.fix[i]   <- ustart
            x.fix[i]   <- xstart + ustart * (tseq[i] - tstart)
          }
        }
        df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
        df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
      }
      tseq.   <- seq(test,tend.0,delt)
      tlen.   <- length(tseq.)
      if(test < tend.0) {
        u.fix   <- x.fix <- rep(NA,tlen.)
        for(i in 1:tlen.) {
          u.fix[i]   <- as.numeric(df1[df1[,1] == tseq.[i],2])
          x.fix[i]   <- as.numeric(df1[df1[,1] == tseq.[i],3]) - hsafe(u.fix[i],leff)
        }
        for(i in 2:tlen.) {
          if(x.fix[i] < x.fix[i-1]) x.fix[i] <- x.fix[i-1]
        }
        if(min(u.fix) < 0) {
          for(i in 1:tlen.) {
            u.fix[i]   <- ustart
            x.fix[i]   <- xstart + ustart * (tseq.[i] - tstart)
          }
        }
        df2.fix[df2.fix[,1] >= test,2] <- u.fix
        df2.fix[df2.fix[,1] >= test,3] <- x.fix
      }
    }
    zone.df <- rbind(zone.df, data.frame(Zone = 3, tstart, tend, ustart, uend, xstart, xend))[-1,]
  }
  # Zone 1 ################################################################################
  if(dim(dfcrit)[1] > 5)  {
    tend     <- tstart
    uend     <- ustart
    xend     <- xstart
    tstart   <- 0
    ustart   <- as.numeric(df2.fix[1,2])
    xstart   <- as.numeric(df2.fix[1,3])
    tseq     <- seq(tstart,tend,delt)
    ab       <- xabparam(tstart, tend, ustart, uend, xstart, xend)
    a        <- ab[1]
    b        <- ab[2]
    t0       <- tstart
    x.fix    <- xab(xstart,ustart,a,b,t = tseq,t0)
    u.fix    <- uab(ustart,a,b,t = tseq,t0)
    df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,2] <- u.fix
    df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tend,3] <- x.fix
  } else {
    df.01 <- data.frame("Zone 1", NA, tstart, tend, ustart, uend, xstart, xend)
    fraction <- seq(1,0,-0.001)
    tend     <- tstart
    uend     <- ustart
    xend     <- xstart
    tstart   <- 0
    ustart   <- as.numeric(df2.fix[1,2])
    xstart   <- as.numeric(df2.fix[1,3])
    tseq     <- seq(tstart,tend,delt)
    tlen     <- length(tseq)
    X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
    tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
    ucrit    <- as.numeric(df1[df1[,1] == tcrit,2])
    xcrit    <- as.numeric(df1[df1[,1] == tcrit,3]) - hsafe(ucrit,leff)
    if(sum(X) > 0) {
      for(j in 1:length(fraction)) {
        tseq     <- seq(tstart,tcrit,delt)
        if(tcrit > tstart) {
          ab       <- xabparam(tstart, tcrit, ustart, ucrit, xstart, xcrit)
          a        <- ab[1]
          b        <- ab[2]
          t0       <- tstart
          x.fix    <- xab(xstart,ustart,a,b,t = tseq,t0)
          u.fix    <- uab(ustart,a,b,t = tseq,t0)
          df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tcrit,2] <- u.fix
          df2.fix[df2.fix[,1] >= tstart & df2.fix[,1] <= tcrit,3] <- x.fix
        }
        tseq       <- seq(tcrit,tend,delt)
        if(tcrit < tend) {
          ab       <- xabparam(tcrit, tend, ucrit, uend, xcrit, xend)
          a        <- ab[1]
          b        <- ab[2]
          t0       <- tcrit
          x.fix    <- xab(xcrit,ucrit,a,b,t = tseq,t0)
          u.fix    <- uab(ucrit,a,b,t = tseq,t0)
          df2.fix[df2.fix[,1] >= tcrit & df2.fix[,1] <= tend,2] <- u.fix
          df2.fix[df2.fix[,1] >= tcrit & df2.fix[,1] <= tend,3] <- x.fix
        }
        X        <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[,1]
        tcrit    <- findviolation(tstart, tend, tend.0, df1[,-1], df2.fix[,-1], delt, leff)[1,6]
        ucrit    <- as.numeric(df1[df1[,1] == tcrit,2])
        xcrit    <- as.numeric(df1[df1[,1] == tcrit,3]) - 1.05 * hsafe(ucrit,leff)
        ustart   <- fraction[j] * ustart
        if(sum(X) == 0) break
      }
    }
  }
  df.01 <- data.frame("Zone 1", tstart, tend, ustart, uend, xstart, xend)
  zone.df <- rbind(data.frame(Zone = 1, tstart, tend, ustart, uend, xstart, xend), zone.df)
  return(df2.fix[,-1])
}
