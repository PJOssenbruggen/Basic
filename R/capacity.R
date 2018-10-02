#' \code{capacity} is an algorithm for estimating the headway from \code{brk4trials} output
#'
#' @return \code{capacity} returns the mean and standard deviation of a capacity estimate.

#' @param df1df2 leading vehicle, a matrix
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param step size in seconds, a number
#' xfunnel upstream location of bottleneck taper, a number
#' @usage capacity(df1df2, tstart, tend, step, xfunnel)
#' @examples
#' capacity(df1df2, tstart, tend, step, xfunnel)
#' @export
capacity <- function(df1df2, tstart, tend, step, xfunnel) {
  nveh   <- dim(df1df2)[2]/3
  t      <- seq(tstart,tend,step)
  df1df2 <- cbind(t, df1df2)
  v <- a <- d <- w <- u.a <- u.d <- cap <- seq(1,nveh)
  for(veh in 1:nveh) {
    xcol   <- 3 + 3*(veh - 1)
    a[veh] <- max(df1df2[df1df2[,xcol] <= xfunnel,1])
    d[veh] <- max(df1df2[df1df2[,xcol] <= 0,1])
    w[veh] <- d[veh] - a[veh]
  }
  for(veh in 1:nveh) {
    ucol     <- 2 + 3*(veh - 1)
    u.a[veh] <- as.numeric(df1df2[df1df2[,1] == a[veh],ucol])*3600/5280
    u.d[veh] <- as.numeric(df1df2[df1df2[,1] == d[veh],ucol])*3600/5280
  }
  df    <- data.frame(vehicle = v, arrival = a, departure = d, delay = w, u.a, u.d)
  cap[1]  <- NA
  for(veh in 2:nveh) {
    xcol   <- 3 + 3*(veh - 1)
    t2     <- max(df1df2[df1df2[,xcol] <= 0,1])
    xcol   <- 3 + 3*(veh - 2)
    t1     <- max(df1df2[df1df2[,xcol] <= 0,1])
    dt     <- t2-t1
    cap[veh] <- 3600/dt
  }
  df <- cbind(df, cap)
  print(round(df,0))
  plot(df[,2], df[,1], typ = "s", xlim = c(0,max(df[,3])), ylim = c(1,nveh+0.1),
       ylab = "Vehicle", xlab = "t, seconds", col = "red", lwd = 2)
  lines(df[,3], df[,1], typ = "s", col = "green", lwd = 2)
  abline(v = 0, col = gray(0.8))
  abline(h = 1, col = gray(0.8))
  legend("topleft", legend = c("A(t)","D(t)"), lty = c(1,1), col = c("red","green"), bty = "n")
  cap <- round(mean(df[,7], na.rm = TRUE),0)
  mtext(bquote(bar(c) == .(cap)))
  u.a.mean.mph <- round(mean(df[,5]),1)
  u.d.mean.mph <- round(mean(df[,6]),1)
  print(data.frame(u.a.mean.mph, u.d.mean.mph))

  browser()
}
