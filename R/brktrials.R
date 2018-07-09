#' \code{brktrials} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{brktrials} returns  \code{t-x} tractories
#' for \code{nveh} vehicles at a bottleneck.
#' \code{k}, \code{xstart} and \code{lane} are input vectors of length \code{nveh}.
#' \code{brktrials} is a wrapper function of \code{accelpass}.
#' @param tend end time for simualted run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a vector
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane number, a vector
#' @param step size in seconds, a number
#' @usage brktrials(tend, umn, usd, xstart, xfunnel, leff, lane, step)
#' @examples
#' brktrials(30, 41, 11, xstart, -500, 14, lane, 0.5)
#' @export
brktrials <- function(tend, umn, usd, xstart, xfunnel, leff, lane, step) {
  lane <- c(0,1,2,1,2)
  xstart <- c(-900,-1000,-1020, -1090, -1100)
  tend.save <- tend
  nveh <- length(lane)
  if(nveh == 1) stop("Warning: lane and xstart should be vectors.")
  # store accelpass output in a list of length nveh
  df      <- accelpass(tend, umn, usd, xstart[1], xfunnel, leff, lane[1], step)
  lst     <- lstoriginal <- list(df[,c(1,5,6,7,8)])
  for(veh in 2:nveh) {
    # accelpass(tend, umn, usd, xstart, xfunnel, leff, lane, step)
    df    <- accelpass(tend, umn, usd, xstart[veh] , xfunnel, leff, lane[veh] , step)
    lst2  <- list(df[,c(1,5,6,7,8)])
    lst   <- rlist::list.append(lst, lst2)
  }
  # conflict test. t0 = time when vehicle i location is x = 0.
  dfcross <- {}
  for(veh in 1:nveh) {
    dftest  <- as.data.frame(lst[[veh]])
    result  <- brkcross0(i = veh, dftest)
    h <- u  <- x <- hobs <- time <- NA
    result  <- c(result, time = time, hsafe = h, u = u, x = x, hobs = hobs)
    dfcross <- rbind(dfcross, result)
  }
  df1  <- as.data.frame(lst[[1]])
  df1  <- cbind(df1, vehicle = rep(1,dim(df1)[1]))
  plot(df1[,1],df1[,3], xlab = "t", ylab = "x", typ = "l",
       xlim = c(0,tend.save), ylim = c(-1120,1000))
  abline(h = c(0, xfunnel), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  nsteps <- dim(df1)[1]
  text(df1[nsteps,1], df1[nsteps,3], labels = "1", pos = 4)
  df1.  <- df1[df1[,3] > xfunnel & df1[,3] < 0,]
  df1.. <- df1[df1[,3] >= 0,]
  df2   <- as.data.frame(lst[[2]])
  df2   <- cbind(df2, vehicle = rep(2,dim(df2)[1]))
  df2.  <- df2[df2[,3] > xfunnel & df2[,3] < 0,]
  df2.. <- df2[df2[,3] >= 0,]
  df3   <- as.data.frame(lst[[3]])
  df3   <- cbind(df3, vehicle = rep(3,dim(df3)[1]))
  df3.  <- df3[df3[,3] > xfunnel & df3[,3] < 0,]
  df3.. <- df3[df3[,3] >= 0,]
  df4   <- as.data.frame(lst[[4]])
  df4   <- cbind(df4, vehicle = rep(4,dim(df4)[1]))
  df4.  <- df4[df4[,3] > xfunnel & df4[,3] < 0,]
  df4.. <- df4[df4[,3] >= 0,]
  df5   <- as.data.frame(lst[[5]])
  df5   <- cbind(df5, vehicle = rep(5,dim(df5)[1]))
  df5.  <- df5[df5[,3] > xfunnel & df5[,3] < 0,]
  df5.. <- df5[df5[,3] >= 0,]
  lstorg <- lst
  for(veh in 2:nveh) {
    # follower
    dfi     <- as.data.frame(lst[[veh]])
    vehicle <- rep(veh, dim(dfi)[1])
    dfi     <- cbind(dfi, vehicle = vehicle)
    tmp <- dfi[dfi[,3] <= xfunnel,]
    lines(tmp[,1], tmp[,3], lwd = 1)
    nstepsi <- dim(dfi)[1]
    time  <- as.numeric(dfcross[veh-1,2])
    dfi.  <- as.data.frame(lst[[veh-1]])
    dfi.  <- cbind(dfi., vehicle = rep(veh-1,dim(df1)[1]))
    tseq. <- dfi.[,1]
    u     <- dfi[tseq. == time,2]
    h     <- hsafe(u, leff)
    hobs  <- as.numeric(dfcross[veh-1,4]) - as.numeric(dfi[tseq. == time,3])
    df..  <- rbind(data.frame(dfi.[tseq. == time,c(1:6)]),
                   data.frame(dfi.[tseq. == time,c(1:6)]))
    rownames(df..) <- c("follower", "leader")
    dfcross[veh,5] <- time
    dfcross[veh,6] <- h
    dfcross[veh,7] <- df..[1,2]
    dfcross[veh,8] <- df..[1,3]
    dfcross[veh,9] <- df..[2,3] - df..[1,3]
    x <- dfcross[veh,8]
    if(as.numeric(dfcross[veh,6]) >= as.numeric(dfcross[veh,9])) {
      print(data.frame(vehicle = veh, status = "Fix"))
      print(df..)
      x <- df..[2,3] - as.numeric(dfcross[veh,6])
      dfcross[veh,8] <- x
      df...     <- as.data.frame(lst[[veh]])
      tseq...   <- df...[,1]
      useq...   <- df...[,2]
      xseq...   <- df...[,3]
      ufunnel...<- max(useq...[xseq... <= xfunnel])
      xfunnel...<- max(xseq...[xseq... <= xfunnel])
      time...   <- tseq...[xseq... == xfunnel...]
      tstartab  <- time...
      tendab    <- as.numeric(dfcross[veh,5])
      ustartab  <- ufunnel...
      uendab    <- as.numeric(dfcross[veh,7])
      xstartab  <- xfunnel...
      xendab    <- as.numeric(dfcross[veh,8])
      ab  <- xabparam(tstart = tstartab, tend = tendab,
                     ustart = ustartab, uend = uendab,
                     xstart = xstartab, xend = xendab)
      a  <- ab[1]
      b  <- ab[2]
      x0 <- xstartab
      u0 <- ustartab
      t0 <- tstartab
      tseq...   <- seq(t0, tendab, step)
      xab... <- uab...  <- yab...  <- {}
      for(i in 1:length(tseq...)) {
        xab... = c(xab..., xab(x0,u0,a,b,t = tseq...[i],t0))
        uab... = c(uab..., uab(u0,a,b,t = tseq...[i],t0))
        yab... = c(yab..., df...[df...[,1] == tseq...[i], 4])
      }
      lines(tseq..., xab..., lwd = 2)
      merge.df <- data.frame(t = tseq..., u = uab..., x = xab...,
                 y = yab...,
                 lane = rep(lane[veh],length(tseq...)),
                 vehicle = rep(veh,length(tseq...)))
      tstart  <- tendab
      xstart  <- xab...[length(xab...)]
      umn     <- 3600/5280*uab...[length(uab...)]
      tux     <- bmfree2(umn, usd, tstart, tend, xstart, step, FALSE)
      tuxlead <- as.data.frame(lst[[veh-1]])
      tuxlead <- tuxlead[tuxlead[,1] >= tstart,]
      lines(tux[,1], tux[,3], col = "red", lty = 3)
      # no passing allowed check
      nope <- cbind(tuxlead[,c(1,2,3)], tux[,c(2,3)])
      colnames(nope) <- c("t", "u.lead","x.lead","u.follow","x.follow")
      downstream     <- nopass(veh, nope, merge.df, leff, step)
      # Fix tailgating
      lines(downstream[,1], downstream[,10], lwd = 1)
      nsteps <- dim(downstream)[1]
      text(downstream[nsteps,1], downstream[nsteps,10], labels = as.character(veh), pos = 4)
      # update lst with downstream
        tcrit <- downstream[1,1]
        tux   <- as.data.frame(lst[[veh]])
        ufix  <- downstream[,9]
        xfix  <- downstream[,10]
        tux[tux[,1] >= tcrit,2] <- ufix
        tux[tux[,1] >= tcrit,3] <- xfix
        lst[[veh]] <- tux
    } else {
      print(data.frame(vehicle = veh, status = "No fix"))
      lines(dfi[,1], dfi[,3], lwd = 2)
      text(dfi[nsteps,1], dfi[nsteps,3], labels = as.character(veh), pos = 4)
      lstdn.[[veh]] <- dfi
    }

  }
  legend("topleft",legend = c("Desired trajectory", "Leader","Leader", "Follower"),
        lty = c(3,1,3,1), lwd = c(1,1,3,1), col = c(gray(0.5),gray(0), gray(0), gray(0)),
        bty = "n")
  title(main = "Bottleneck Merge", sub = "Brownian Motion Model")
  return(list(dfcross, lst, lstorg))
}
