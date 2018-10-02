#' The function \code{safecrossover2}  driver conflict of vehicles changing lanes.
#'
#' @param umn average speed in mph, a number
#' @param leff effective vehicle length in feet, a number
#' @usage safecrossover2(umn, leff)
#' @examples
#' safecrossover2(67, 14)
#' @export
safecrossover2 <- function(umn, leff) {
  x <- y <- 0
  xstart <- -850
  xend   <- 250

  u0     <- umn * 5280/3600
  tend   <- (xend-xstart)/u0
  tseq   <- seq(0,tend,0.1)
  xseq   <- xab(xstart,u0,0,0,tseq,0)
  tlen   <- length(tseq)
  y      <- rep(NA, tlen)
  df     <- data.frame(t = tseq, x = xseq, y)
  for(i in 1: tlen) {
    if(df[i,2] >= 0) df[i,3] <- -6
    if(df[i,2] <= -500) df[i,3] <- 6
    if(-500 < df[i,2] & df[i,2] < 0) df[i,3] <- -12/500 * df[i,2] - 6
  }
  Vehicle <- rep(1, dim(df)[1])
  u       <- rep(u0, dim(df)[1])
  df      <- cbind(df, Vehicle, u)
  browser()
  # Vehicle 2 decelerate
  tstart <- max(df[df[,2] <= -500,1])
  ustart <- u0
  xstart <- max(df[df[,2] <= -500,2])
  tend   <- max(df[df[,2] <= -250,1])
  uend   <- u0
  xend   <- max(df[df[,2] <= -250,2]) - 2*leff
  ab     <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  tseq2  <- seq(tstart,tend,0.1)
  tlen2  <- length(tseq2)
  u2     <- uab(u0, a = ab[1], b = ab[2], t = tseq2, t0 = tstart)
  x2     <- xab(x0 = xstart, u0, a = ab[1], b = ab[2], t = tseq2, t0 = tstart)
  df2a   <- data.frame(t = tseq2, u = u2, x = x2)
  # Vehicle 2 accelerate
  tstart <- tend
  ustart <- u0
  xstart <- xend
  tend   <- max(df[df[,2] <= 0,1])
  uend   <- u0
  xend   <- max(df[df[,2] <= 0,2])
  ab     <- xabparam(tstart, tend, ustart, uend, xstart, xend)
  tseq3  <- seq(tstart,tend,0.1)
  tlen3  <- length(tseq3)
  u3     <- uab(u0, a = ab[1], b = ab[2], t = tseq3, t0 = tstart)
  x3     <- xab(x0 = xstart, u0, a = ab[1], b = ab[2], t = tseq3, t0 = tstart)
  df2b   <- data.frame(t = tseq3, u = u3, x = x3)
  df2ab  <- rbind(df2a,df2b[-1,])
  df2    <- df
  points(0, df2a[dim(df2a)[1],3])
  tstart <- df2a[1,1]
  tend   <- df2b[dim(df2b)[1],1]
  df2[df2[,1] >= tstart & df2[,1] <= tend,2] <- df2ab[,3]
  Vehicle <- rep(2, dim(df2)[1])
  tbegin  <- tseq[tseq < tstart]
  ubegin  <- rep(u0,length(tbegin))
  tup     <- tseq[tseq > tend]
  uup     <- rep(u0,length(tup))
  u       <- c(ubegin, df2ab[,2], uup)
  df2[,4] <- Vehicle
  df2[,5] <- u
  df      <- rbind(df,df2)
  df      <- df[df[,1] >= 3 & df[,1] <= 9,]
  return(df)
}
