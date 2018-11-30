#' \code{flow} is an algorithm for estimating the flow and density
#'
#' @return \code{flow} returns data.frame of density and flow estimates at two bottleneck locations.
#' @param df1df2 vehicle information from \code{brktrials4wrapper}, a matrix
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param step size in seconds, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @usage flow(df1df2, tstart, tend, step, xfunnel)
# #' @examples
# #' flow(df1df2, tstart, tend, step, xfunnel)
#' @export
flow <- function(df1df2, tstart, tend, step, xfunnel) {
  nveh     <- dim(df1df2)[2]/3
  t        <- seq(tstart,tend,step)
  df1df2   <- cbind(t, df1df2)
  ucol     <- seq(2, dim(df1df2)[2],3)
  u.df     <- df1df2[,ucol]
  u.mn     <- colMeans(u.df, na.rm = TRUE)
  u.mn     <- mean(u.mn)*3600/5280
  v <- a <- d <- w <- u.a <- u.d <- x.a <- x.d <- x.a.f <- x.d.f <- seq(1,nveh)
  for(veh in 1:nveh) {
    xcol     <- 3 + 3*(veh - 1)
    a[veh]   <- max(df1df2[df1df2[,xcol] <= xfunnel,1])
    x.a[veh] <- max(df1df2[df1df2[,1] <= a[veh],xcol])
    if(xcol+3 > dim(df1df2)[2]) x.a.f[veh] <- NA else x.a.f[veh] <- max(df1df2[df1df2[,1] <= a[veh],xcol+3])
    d[veh]   <- max(df1df2[df1df2[,xcol] <= 0,1])
    x.d[veh] <- max(df1df2[df1df2[,1] <= d[veh],xcol])
    if(xcol+3 > dim(df1df2)[2]) x.d.f[veh] <- NA else x.d.f[veh] <- max(df1df2[df1df2[,1] <= d[veh],xcol+3])
    w[veh]   <- d[veh] - a[veh]
  }
  for(veh in 1:nveh) {
    ucol     <- 2 + 3*(veh - 1)
    u.a[veh] <- as.numeric(df1df2[df1df2[,1] == a[veh],ucol])*3600/5280
    u.d[veh] <- as.numeric(df1df2[df1df2[,1] == d[veh],ucol])*3600/5280
  }
  df1      <- data.frame(vehicle = v, arrival = a, departure = d, delay = w, u.a, u.d, x.a,x.d,x.a.f,x.d.f)
  flow     <- rep(NA,nveh)
  for(veh in 2:nveh) {
    xcol   <- 3 + 3*(veh - 1)
    t2     <- max(df1df2[df1df2[,xcol] <= 0,1])
    xcol   <- 3 + 3*(veh - 2)
    t1     <- max(df1df2[df1df2[,xcol] <= 0,1])
    dt     <- t2-t1
    flow[veh] <- 3600/dt
  }
  flow         <- round(mean(flow, na.rm = TRUE),0)
  u.a.mean.mph <- round(mean(df1[,5]),1)
  u.d.mean.mph <- round(mean(df1[,6]),1)
  df2 <- data.frame(u.a.mean.mph, u.d.mean.mph, flow, u.mn)
  sa  <- abs(df1[c(1:nveh-1),7]-df1[c(1:nveh-1),9])
  sd  <- abs(df1[c(1:nveh-1),8]-df1[c(1:nveh-1),10])
  q.a <- abs(3600/diff(df1[,2]))
  q.d <- abs(3600/diff(df1[,3]))
  df3 <- data.frame(interval = seq(1,nveh-1), q.a, k.a = 5280/sa, q.d, k.d = 5280/sd)
  return(list(df1,df2,df3))
}
