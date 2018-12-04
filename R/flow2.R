#' \code{flow2} is an algorithm for estimating the flow and density
#'
#' @return \code{flow2} returns data.frame of density and flow estimates at two bottleneck locations.
#' @param dfcrit times when vehicles arrive at bottleneck by lane, a matrix
#' @param df1df2 vehicle information from \code{brktrials4wrapper}, a matrix
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param delt size in seconds, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @usage flow2(dfcrit, df1df2, tstart, tend, delt, xfunnel)
# #' @examples
# #' flow2(dfcrit, df1df2, tstart, tend, delt, xfunnel)
#' @export
flow2 <- function(dfcrit, df1df2, tstart, tend, delt, xfunnel) {
  nveh     <- dim(df1df2)[2]/3
  t        <- seq(tstart,tend,delt)
  df1df2   <- cbind(t, df1df2)
  ucol     <- seq(2, dim(df1df2)[2],3)
  u.df     <- df1df2[,ucol]
  u.mn     <- colMeans(u.df, na.rm = TRUE)
  u.mn.veh <- u.mn*3600/5280
  u.mn.all <- mean(u.mn)*3600/5280
  v <- a <- d <- w <- u.a <- u.d <- x.a <- x.d <- x.a.f <- x.d.f <- seq(1,nveh)
  # Arrival times by vehicle at x = 0 and xfunnel
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
  # Arrival times by vehicle and lane at x = 0 and xfunnel
  nveh1 <- sum(as.numeric(dfcrit[dfcrit[,1] == 1,1]))
  nveh2 <- sum(as.numeric(dfcrit[dfcrit[,1] == 2,1]))/2
  x.a1  <- x.d1 <- u.a1  <- u.d1 <- seq(1,nveh1)
  x.a2  <- x.d2 <- u.a2  <- u.d2 <- seq(1,nveh2)
  a1    <- a[dfcrit[,1] == 1]
  a2    <- a[dfcrit[,1] == 2]
  w1 <- d1    <- a1
  w2 <- d2    <- a2
  index  <- matrix(seq(2,dim(df1df2)[2]),ncol = 3, byrow = TRUE)
  index  <- cbind(dfcrit[,1], index)
  index1 <- as.numeric(index[as.numeric(index[,1]) == 1,c(2:4)])
  o      <- order(index1)
  index1 <- index1[o]
  index1
  index2 <- as.numeric(index[as.numeric(index[,1]) == 2,c(2:4)])
  o      <- order(index2)
  index2 <- index2[o]
  index2
  lane1     <- df1df2[,index1]
  lane2     <- df1df2[,index2]
  x1df1df2  <- lane1[,seq(2,length(index1),3)]
  t         <- as.matrix(data.frame(t))
  x1df1df2  <- cbind(t, x1df1df2)
  x2df1df2  <- lane2[,seq(2,length(index2),3)]
  x2df1df2  <- cbind(t,x2df1df2)
  u1df1df2  <- lane1[,seq(1,length(index1),3)]
  u1df1df2  <- cbind(t,u1df1df2)
  u2df1df2  <- lane2[,seq(1,length(index2),3)]
  u2df1df2  <- cbind(t,u2df1df2)
  for(veh in 1:nveh1) {
    xcol      <- 1 + veh
    a1[veh]   <- max(x1df1df2[x1df1df2[,xcol] <= xfunnel,1])
    x.a1[veh] <- max(x1df1df2[x1df1df2[,1] <= a1[veh],xcol])
    if(xcol+3 > dim(x1df1df2)[2]) x.a.f[veh] <- NA else
      x.a.f[veh] <- max(x1df1df2[x1df1df2[,1] <= a1[veh],xcol+3])
    d1[veh]   <- max(x1df1df2[x1df1df2[,xcol] <= 0,1])
    x.d1[veh]  <- max(x1df1df2[x1df1df2[,1] <= d1[veh],xcol])
    if(xcol+3 > dim(x1df1df2)[2]) x.d.f[veh] <- NA else
      x.d.f[veh] <- max(x1df1df2[x1df1df2[,1] <= d1[veh],xcol+3])
    w1[veh]   <- d1[veh] - a1[veh]
  }
  for(veh in 1:nveh2) {
    xcol      <- 1 + veh
    a2[veh]   <- max(x2df1df2[x2df1df2[,xcol] <= xfunnel,1])
    x.a2[veh] <- max(x2df1df2[x2df1df2[,1] <= a2[veh],xcol])
    if(xcol+3 > dim(x2df1df2)[2]) x.a.f[veh] <- NA else
      x.a.f[veh] <- max(x2df1df2[x2df1df2[,1] <= a2[veh],xcol+3])
    d2[veh]   <- max(x2df1df2[x2df1df2[,xcol] <= 0,1])
    x.d2[veh]  <- max(x2df1df2[x2df1df2[,1] <= d2[veh],xcol])
    if(xcol+3 > dim(x2df1df2)[2]) x.d.f[veh] <- NA else
      x.d.f[veh] <- max(x2df1df2[x2df1df2[,1] <= d2[veh],xcol+3])
    w2[veh]   <- d2[veh] - a2[veh]
  }
  # Speeds by vehicle number
  for(veh in 1:nveh1) {
    ucol     <- 2 + 3*(veh - 1)
    u.a[veh] <- as.numeric(df1df2[df1df2[,1] == a[veh],ucol])*3600/5280
    u.d[veh] <- as.numeric(df1df2[df1df2[,1] == d[veh],ucol])*3600/5280
  }
  df1        <- data.frame(vehicle = v, arrival = a, departure = d, delay = w)
  df1        <- cbind(dfcrit[,c(1,3,4)],df1[,-1])
  # Density
  xcol          <- seq(3, dim(df1df2)[2],3)
  xdf1df2       <- df1df2[,c(1,xcol)]
  departimes    <- df1[,5]
  s0            <- matrix(xdf1df2[df1df2[,1] == departimes[1],])
  # Headways for departures
  for(i in 2:nveh) {
    sadd <- matrix(xdf1df2[xdf1df2[,1] == departimes[i],])
    s0   <- cbind(s0,sadd)
  }
  s0  <- abs(t(s0))
  s0  <- s0[,-1]
  sii <- rep(NA,nveh-1)
  sij <- rep(NA,nveh-1)
  for(i in 1:nveh-1) sii[i] <- s0[i,i]
  for(i in 1:nveh-1) sij[i] <- s0[i,i+1]
  s0diff <- sii[-nveh] - sij
  k.d <- c(abs(5280/s0diff),NA)
  q.d <- c(3600/diff(d),NA)
  df1 <- cbind(df1,k.d,q.d,u.d)
  for(veh in 1:nveh) {
    ucol     <- 2 + 3*(veh - 1)
    u.a[veh] <- as.numeric(df1df2[df1df2[,1] == a[veh],ucol])*3600/5280
    u.d[veh] <- as.numeric(df1df2[df1df2[,1] == d[veh],ucol])*3600/5280
  }
  df1        <- data.frame(vehicle = v, arrival = a, departure = d, delay = w)
  df1        <- cbind(dfcrit[,c(1,3,4)],df1[,-1])
  # Headways for Lane 1 arrivals
  s0            <- matrix(x1df1df2[df1df2[,1] == a1[1],])
  for(i in 2:nveh1) {
    sadd <- matrix(x1df1df2[x1df1df2[,1] == a1[i],])
    s0   <- cbind(s0,sadd)
  }
  s0  <- abs(t(s0))
  s0  <- s0[,-1]
  sii <- rep(NA,nveh1-1)
  sij <- rep(NA,nveh1-1)
  for(i in 1:nveh1-1) sii[i] <- s0[i,i]
  for(i in 1:nveh1-1) sij[i] <- s0[i,i+1]
  s0diff <- sii[-nveh1] - sij
  k.d1 <- c(abs(5280/s0diff),NA)
  q.d1 <- c(3600/diff(d1),NA)
  # Headways for Lane 2 arrivals
  s0            <- matrix(x2df1df2[df1df2[,1] == a2[1],])
  for(i in 2:nveh2) {
    sadd <- matrix(x2df1df2[x2df1df2[,1] == a2[i],])
    s0   <- cbind(s0,sadd)
  }
  s0  <- abs(t(s0))
  s0  <- s0[,-1]
  sii <- rep(NA,nveh2-1)
  sij <- rep(NA,nveh2-1)
  for(i in 1:nveh2-1) sii[i] <- s0[i,i]
  for(i in 1:nveh2-1) sij[i] <- s0[i,i+1]
  s0diff <- sii[-nveh2] - sij
  k.d2 <- c(abs(5280/s0diff),NA)
  q.d2 <- c(3600/diff(d2),NA)
  q.a <- c(q.d1,q.d2)
  k.a <- c(k.d1,k.d2)
  df1 <- cbind(df1,k.d,q.d,u.d,k.a,q.a,u.a)
  return(df1)
}
