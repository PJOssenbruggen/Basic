#' The function \code{brksummary} is a wrapper for  \code{accelpass} and \code{passplot}.
#'
#' @return \code{brksummary} contains a table of information used for \code{run} plot
#' @param k traffic density (vehicles per mile, vpm), a number
#' @param tend end time, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a number
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane number, a number
#' @param step size in seconds, a number
#' @usage brksummary(k, tend, umn, usd, xstart, xfunnel, leff, lane, step)
#' @examples
#' brksummary(50, 30, 41, 11, -1000, -500, 14, 0, 2)
#' @export
brksummary <- function(k, tend, umn, usd, xstart, xfunnel, leff, lane, step) {
  set.seed(123)
# accelpass(tend, umn, usd, xstart, xfunnel, leff, lane, step)
  df0 <- accelpass(30, 41, 11, -1000, -500,  14, 0, step)
  # vehicle 0 is ahead of vehicles 1 and 2. adjust x0
  l <- 5280/k
  tend.save <- tend
  df0[,3] <- rep(l, length(df0[,1])) + df0[,3]
  df0[,6] <- rep(l, length(df0[,1])) + df0[,6]
  df0 <- df0[,c(1,5,6,7,8)]
  df1 <- accelpass(30, 41, 11, -1000, -500,  14, 1, step)
  df1 <- df1[,c(1,5,6,7,8)]
  df2 <- accelpass(30, 41, 11, -1000, -500,  14, 2, step)
  df2 <- df2[,c(1,5,6,7,8)]
  df012   <- cbind(df0,df1,df2)
  df <- matrix(rep(NA,24), nrow = 4)
  colnames(df) = c("tstart", "tend", "ustart", "uend", "xstart","xend")
  rownames(df) <- c("vehicle 0", "vehicle 1", "vehicle 2 conflict", "vehicle 2")
# df is a data frame for the merge zone.
  # lane 0
  df[1,1] <- max(df012[df012[,3] < xfunnel,1])   # tstart = t at x = xfunnel
  df[1,2] <- max(df012[df012[,3] < 0,1])         # tend = t0 at x = x0
  df[1,3] <- df012[df012[,1] == df[1,1],2]       # ustart = u at x = xfunnel
  df[1,4] <- df012[df012[,1] == df[1,2],2]       # uend = u at x = x0
  df[1,5] <- df012[df012[,1] == df[1,1],3]       # xstart = x at x = xfunnel
  df[1,6] <- df012[df012[,1] == df[1,2],3]       # xend = x at x = x0
  # lane 1
  df[2,1] <- max(df012[df012[,8] < xfunnel,6])   # tstart = t at x = xfunnel
  df[2,2] <- max(df012[df012[,8] < 0,6])         # tend = t0 at x = x0
  df[2,3] <- df012[df012[,1] == df[2,1],7]       # ustart = u at x = xfunnel
  df[2,4] <- df012[df012[,1] == df[2,2],7]       # uend = u at x = x0
  df[2,5] <- df012[df012[,1] == df[2,1],8]       # xstart = x at x = xfunnel
  df[2,6] <- df012[df012[,1] == df[2,2],8]       # xend = x at x = x0
  # lane 2 pink conflict
  df[3,1] <- max(df012[df012[,13] < xfunnel,11])  # tstart = t at x = xfunnel
  df[3,2] <- max(df012[df012[,13] < 0,11])        # tend = t0 at x = x0
  df[3,3] <- df012[df012[,1] == df[3,1],12]       # ustart = u at x = xfunnel
  df[3,4] <- df012[df012[,1] == df[3,2],12]       # uend = u at x = x0
  df[3,5] <- df012[df012[,1] == df[3,1],13]       # xstart = x at x = xfunnel
  df[3,6] <- df012[df012[,1] == df[3,2],13]       # xend = x at x = x0
  # lane 2 red hsafe
  df[4,1] <- df[3,1]
  df[4,2] <- df[3,2]
  df[4,3] <- df[3,3]
  df[4,4] <- df[3,4]
  df[4,5] <- df[3,5]
  df[4,6] <- df[3,6]
  # vehicles 0 and 1
  plot(df012[,1],df012[,3], typ = "l", xlab = "t", ylab = expression(x[t]),
       col = "orange", ylim = c(xstart,max(df0[,3])))
  data.frame(df012[,1],df012[,7],df012[,8])
  lines(df012[,1],df012[,8], col = "blue")
  abline(h = c(0,-500,-1000), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  abline(v = df[,2], lty = 2)
  axis(side = 3, at = df[c(1,2,3),2], labels = c(
    expression(t[0]),
    expression(t[1]),
    expression(t[2])
    ))
  legend("topleft", legend = c("vehicle 0", "vehicle 1", "vehicle 2", "vehicle 2 conflict"),
         lty = c(1,1,1,1), col = c("orange","blue", "red","pink"), bty = "n" )
  # veh 2 is the follower. Find xab between xstart and xfunnel
  tstart  <- 0
  tend    <- df[3,1]
  tseq    <- seq(tstart, tend, step)
  index   <- seq(1, length(tseq))
  xseq    <- df012[index, 8]
  lines(df012[,1],df012[,13], col = "pink")
  # Determine vehicle 2 correction
  u       <- min(c(df[2,4], df[3,4]))       # u for vehicle 2 that cannot exceed u for vehicle 1
  hsafe2  <- hsafe(u, leff)
  df[4,6] <- df[2,6] - hsafe2
  if(df[2,2] < df[3,2]) {
    df[4,2] <- df[2,2]
    df[4,4] <- df[2,4]
    df[4,6] <- df[2,6] - hsafe(df[4,4], leff)
    points(df[4,2], df[2,6], pch = 16, col = "blue")
    points(df[4,2], df[4,6], pch = 16, col = "red")
  } else {
    df[4,2] <- df[3,2]
    df[4,4] <- df[3,4]
    df[4,6] <- df[3,6] - hsafe(df[4,4], leff)
    points(df[4,2], df[2,6], pch = 16, col = "blue")
    points(df[4,2], df[4,6], pch = 16, col = "red")
  }
  # veh 2 is the follower. Find xab between xfunnel and x = 0
  tstart <- df[4,1]
  tend   <- df[4,2]
  ustart <- df[4,3]
  uend   <- df[4,4]
  xstart <- df[4,5]
  xend   <- df[4,6]
  ab     <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  x0     <- df[4,5]
  u0     <- df[4,3]
  tab    <- seq(tstart, tend, step)
  t.up   <- seq(0, tstart-step, step)
  t0     <- df[4,1]
  xabfollow <- xab(x0,u0,a = ab[1],b = ab[2],t = tab,t0)
  uabfollow <- uab(u0,a = ab[1],b = ab[2],t = tab,t0)
  lines(tab, xabfollow)
  # downstream t-x trajectory
  tstart <- df[4,2]
  tend   <- tend.save
  umn    <- df[4,3]
  usd    <- usd * 5280/3600
  xstart <- df[4,6]
  # acceldown(tstart, tend, umn, usd, xstart,  step)
  dfdown <- acceldown(tstart, tend, umn, usd, xstart, step)
  lines(dfdown[,1], dfdown[,3], col = "red")
  # Output a data frame suitable for passplot(df, title)
  df12.  <- df012[,c(6:10)]
  colnames(df12.) <- c("t","u","x","y","lane")
  df12..  <- df012[,c(11:15)]
  colnames(df12..) <- c("t","u","x","y","lane")
  df12  <- rbind(df12., df12..)
  df12[,5] <- factor(df12[,5])
  return(list(df, df12))
}
