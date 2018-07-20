#' \code{brktrials2} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{brktrials2} returns  \code{t-x} tractories of \code{nveh} vehicles at a bottleneck.
#' @param nveh number of vehicles in the simulation, a number
#' @param tend end time for simualted run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart a vector of  \code{nveh} start locations, (feet), a vector
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane a vector of \code{nveh} numbers, a vector
#' @param step size in seconds, a number
#' @param type 0 no plots, 1 prediction plot, 2 all plots, a number
#' @usage brktrials2(nveh, tend, umn, usd, xstart, xfunnel, leff, lane, step, type)
#' @examples
#' brktrials2(5, 30, 41, 11, xstart, -500, 14, lane, 0.5, 0)
#' @export
brktrials2 <- function(nveh, tend, umn, usd, xstart, xfunnel, leff, lane, step, type) {
  set.seed(127)
  if(type == 2) {
    layout(matrix(c(1,1,2,3,4,4,5,5), 4,2, byrow = TRUE))
  } else {
    par(mfrow = c(1,1), pty = "m")
  }
  tend.save  <- tend
  lane.      <- lane
  tseq       <- seq(0,tend,step)
  tlen       <- length(tseq)
  # store bmfree2 output in a data frame "df". Dimension: tlen by  nveh.
  tstart <- 0
  for(veh in 1:nveh) {
    # bmfree2(umn, usd, tstart, tend, xstart, step, type)
    df.     <- bmfree2(umn, usd, tstart, tend, xstart[veh], step, type = FALSE)
    vehicle <- rep(veh, tlen)
    y       <- rep(NA, tlen)
    lane    <- rep(lane.[veh], tlen)
    df.     <- cbind(df., y, lane, vehicle)
    if(veh == 1) df <- df. else df <- cbind(df, df.)
  }
  for(veh in 1:nveh) {
    df.   <- vehdf(veh, nveh, df)
    x.    <- vehdf(veh, nveh, df)[,3]
    y.    <- vehdf(veh, nveh, df)[,4]
    lane  <- vehdf(veh, nveh, df)[,5]
    for(t in 1:tlen) {
      if(as.numeric(df.[t,3]) <= xfunnel & as.numeric(df.[t,5]) == 1) df.[t,4] = -6
      if(as.numeric(df.[t,3]) <= xfunnel & as.numeric(df.[t,5]) == 2) df.[t,4] = 6
      if(as.numeric(df.[t,3]) > 0) df.[t,4] = 0
      if(as.numeric(df.[t,3]) > xfunnel & as.numeric(df.[t,3]) <= 0 &
         as.numeric(df.[t,5]) == 1)
        df.[t,4] = -6/xfunnel * as.numeric(df.[t,3])
      if(as.numeric(df.[t,3]) > xfunnel & as.numeric(df.[t,3]) <= 0 &
        as.numeric(df.[t,5]) == 2)
        df.[t,4] = 6/xfunnel * as.numeric(df.[t,3])
    }
    ufix <- df.[,2]
    xfix <- df.[,3]
    yfix <- df.[,4]
    df   <- vehfix(veh, nveh, ufix, xfix, yfix, df)
  }
  if(type == 2) {
    plotoptimize(df, xfunnel, type)
    title("Driver Conflicts")
  }
  # df0 = with driver constraints
  df0 <- df
#  browser()
  # dfcross = data frame t0 = times when vehicle i location is x = 0.
  dfcross <- {}
  for(veh in 1:nveh) {
    dfij    <- vehdf(veh, nveh, df)
    result  <- brkcross0(veh, dfij)
    h <- u  <- x <- hobs <- time <- NA
    result2 <- data.frame(time = time, hsafe = h, u = u, x = x, hobs = hobs)
    result  <- c(result, result2)
    dfcross <- rbind(dfcross, result)
  }
  colnames(dfcross) <- c("vehicle","tl0","ul0","xl0","tf0","uf0","xf0","hsafef","hobs")
  rownames(dfcross) <- paste("", sep = "",1:nveh)
  # plot Lane 1 and 2 trajectories
  if(type == 2) type. <- 2 else type. <- 0
  pick <- 1
#  browser()
  df   <- plotupstream(pick, lane., nveh, df, xfunnel, leff, type = type.)
  pick <- 2
  df   <- plotupstream(pick, lane., nveh, df, xfunnel, leff, type = type.)
#  browser()
  # plot car-following
  df <-  plotoptimize(df, xfunnel, type)

#  browser()

  # Model forecast plot
  for(veh in 1:nveh) {
    dfij    <- vehdf(veh, nveh, df)
    dfij0   <- dfij[dfij[,3] < 0,]
    index   <- dim(dfij0)[1]
    dfij0   <- as.numeric(dfij0[index,])
    dfx0.   <- data.frame(t = dfij0[1], u = dfij0[2], x = dfij0[3], vehicle = veh)
    if(veh > 1) dfx0 <- rbind(dfx0, dfx0.) else dfx0 = dfx0.
  }
  o <- order(dfx0[,1])
  dfx0 <- dfx0[o,]
  vehorder <- dfx0[,4]
  dfcross  <- dfcross[o,]
  xlimit <- vehdf(1, nveh, df)[,3]
  for(i in 2:nveh) {
    xlimit <- c(xlimit, vehdf(i, nveh, df)[,3])
  }
  ylim <- c(min(xlimit), max(xlimit))
  for(i in 1:nveh) {
    if(i == 1) {
      veh     <- vehorder[i]
      dfij    <- vehdf(veh, nveh, df)
      if(dfij[1,5] == "1") {
        plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
             xlim = c(0,tend.save), ylim = ylim)

      } else {
        plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
             xlim = c(0,tend.save), ylim = ylim, col = "blue")

      }

      abline(h = c(0, xfunnel), col = gray(0.8))
      abline(v = 0, col = gray(0.8))
      text(dfij[tlen,1], dfij[tlen,3], labels = as.character(veh), pos = 4, cex = 1)
    } else {
      veh     <- vehorder[i]
      dfij    <- vehdf(veh, nveh, df)
      if(dfij[1,5] == "1") lines(dfij[,1], dfij[,3], lty = 4)
      else  lines(dfij[,1], dfij[,3], lty = 4, col = "blue")
    }
  }
#      browser()
  # Filter the upstream data to assure drivers conform to safe driving hsafe rules.
  for(i in 2:nveh) {
    # follower
    veh     <- vehorder[i]
    dfij    <- vehdf(veh, nveh, df)
    tf0     <- as.numeric(dfcross[i-1,2])
    uf0     <- as.numeric(dfij[dfij[,1] == tf0, 2])
    xf0     <- dfij[dfij[,1] == tf0, 3]
    hsafef  <- hsafe(uf0, leff)
    hobs    <- as.numeric(dfcross[i-1,4]) - xf0
    dfcross[i,5]  <- tf0
    dfcross[i,6]  <- uf0
    dfcross[i,7]  <- xf0
    dfcross[i,8]  <- hsafef
    dfcross[i,9]  <- hobs
    # Check
    if(as.numeric(dfcross[i,8]) <= as.numeric(dfcross[i,9])) {
    # no safety violation for xab.
      dfij      <- vehdf(veh, nveh, df)
      dfij0     <- dfij[dfij[,1] <= dfcross[i,5],]
      if(dfij0[1,5] == "1") {
        lines(dfij0[,1], dfij0[,3])
      } else {
        lines(dfij0[,1], dfij0[,3], col = "blue")
      }
      tindex    <- as.numeric(dfcross[i,5])
      tux       <- dfij[dfij[,1] >= tindex,]
      # check downstream for hsafe violations
      tuxlead   <- vehdf(veh = vehorder[i-1], nveh, df)
      tuxlead   <- tuxlead[tuxlead[,1] >= tindex,]
      # no passing allowed check
      if(is.null(dim(tux))) nope <- c(tuxlead[c(1,2,3)], tux[c(2,3)])
      else {
        nope    <- cbind(tuxlead[,c(1,2,3)], tux[,c(2,3)])
        colnames(nope) <- c("t", "u.lead","x.lead","u.follow","x.follow")
      }
      tuxfix     <- nopass(veh, nope, leff)
      # Fix tailgating
      if(is.data.frame(tuxfix)) {
        if(dfij0[1,5] == "1") {
          lines(tux[,1], tuxfix[,2], lwd = 1)
        } else {
          lines(tux[,1], tuxfix[,2], lwd = 1, col = "blue")
        }
        nsteps <- dim(tuxfix)[1]
        text(tux[nsteps,1], tuxfix[nsteps,2],
             labels = as.character(veh), pos = 4, cex = 1)
      } else {
        text(tend, tux[3],
             labels = as.character(veh), pos = 4, cex = 1)
      }
#      browser()
    } else {
      # Safe headway violation
      dfij            <- vehdf(veh, nveh, df)
      dfijab          <- dfij[dfij[,3] >= xfunnel & dfij[,3] <= 0,]
      index           <- dim(dfijab)[1]
      if(index > 0) {
        dfcross[i,5]    <- tindex <- as.numeric(dfijab[index,1])
        dfcross[i,6]    <- as.numeric(dfijab[1,2])
        dfcross[i,7]    <- as.numeric(dfijab[1,2])
        dfcross[i,8]    <- hsafe(as.numeric(dfijab[1,2]), leff)
        dfijlead        <- vehdf(veh-1, nveh, df)
        hobs            <- as.numeric(dfijlead[dfijlead[,1] == tindex,3])
        dfcross[i,9]    <- hobs
        dfijfun         <- dfij[dfij[,3] <= xfunnel,]
#        browser()
        lines(dfijfun[,1], dfijfun[,3], lwd = 1)
        index           <- dim(dfijfun)[1]
        tstartab        <- dfijfun[index,1]
        ustartab        <- dfijfun[index,2]
        xstartab        <- dfijfun[index,3]
        tendab          <- as.numeric(dfcross[i,5])
        uendab          <- as.numeric(dfcross[i,6])
        xendab          <- as.numeric(dfcross[i,7])
        ab              <- xabparam(tstart = tstartab, tend = tendab,
                              ustart = ustartab, uend = uendab,
                              xstart = xstartab, xend = xendab)
        a               <- ab[1]
        b               <- ab[2]
        x0              <- xstartab
        u0              <- ustartab
        t0              <- tstartab
        tseq            <- seq(tstartab, tendab, step)
        xmab <- uab  <- yab  <- {}
        for(j in 1:length(tseq)) {
          xmab <- c(xmab, xab(x0,u0,a,b,t = tseq[j],t0))
          uab  <- c(uab, uab(u0,a,b,t = tseq[j],t0))
          yab  <- c(yab, df[df[,1] == tseq[j], 4])
        }
        xabpoints <- rbind(data.frame(t = tstartab, u = ustartab, x = xstartab),
                           data.frame(t = tendab, u = uendab, x = xendab)
        )
#        browser()
        lines(tseq, xmab, lwd = 1)
        tstart  <- tendab
        xstart  <- xmab[length(xmab)]
        umn     <- 3600/5280*uab[length(uab)]
        if(tstart < tend) {
          tux     <- bmfree2(umn, usd, tstart, tend, xstart, step, FALSE)
          lines(tux[,1],tux[,3])
          tuxlead <- vehdf(veh = vehorder[i - 1], nveh, df)
          tuxlead <- tuxlead[tuxlead[,1] >= tstart,]
          # no passing allowed check
          nope           <- cbind(tuxlead[,c(1,2,3)], tux[,c(2,3)])
          colnames(nope) <- c("t", "u.lead","x.lead","u.follow","x.follow")
          tuxfix     <- nopass(veh, nope, leff)
          # Fix tailgating
          lines(tux[,1], tuxfix[,2], lwd = 1)
          nsteps <- dim(tuxfix)[1]
          text(tux[nsteps,1], tuxfix[nsteps,2],
               labels = as.character(veh), pos = 4, cex = 1)
        }
      } else {
        if(dfij[1,5] == "1") {
          lines(dfij[,1], dfij[,3])
          text(dfij[tlen,1], dfij[tlen,3], labels = veh, pos = 4)
        } else {
          lines(dfij[,1], dfij[,3], col = "blue")
          text(dfij[tlen,1], dfij[tlen,3], labels = veh, pos = 4)
        }
      }
    }
  }
  if(type != 0) {
    title(main = "Model Predictions")
    legend("topleft", legend = c(
      "Leading vehicle",
      "Following vehicles",
      "Lane 1", "Lane 2"
    ),
    lty = c(1,4,1,1),
    col = c("black","black","black","blue"),
    bty = "n")
  }
  return(list(dfcross, df, df0))
}
