#' \code{merge3} produces \code{t-x} trajectories for a pair of lead and following vehicles for \code{brktrial3}.
#'
#' @return The \code{merge3}, a wrapper function for \code{bmfree2}, \code{xabparam} and
#' \code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} tractory
#' for the following vehicle. The lead vehicle trajectory is not affected.
#' @param df1 leading vehicle, a matrix
#' @param df2 following vehicle, a matrix
#' @param leff vehicle length, a number
#' @param step time-step size, a number
#' @param xfunnel upstream location of bottleneck taper, a number
#' @param usd speed volatility (mph) for \code{umn}, a number
#' @param ylim for plot, a vector
#' @usage merge3(df1,df2,leff,step,xfunnel,usd,ylim)
# #' @examples
# #' merge3(df1,df2,leff,step,xfunnel,usd,ylim)
#' @export
merge3 <- function(df1, df2,leff,step,xfunnel,usd,ylim) {
  plot(df1[,1],df1[,3], typ  = "l", ylim = ylim,
       xlab = "t, seconds", ylab = expression(x[t]*", feet"))
  lines(df2[,1],df2[,3], lty = 2, col = "orange", lwd = 3)
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, xfunnel), col = gray(0.8))
  k       <- 0
  df1     <- df1[,c(1,2,3)]
  df2     <- df2[,c(1,2,3)]
  # df2 must merge at x = 0 safely behind df1
  t1      <- df1[df1[,3] <= 0,1]
  t1len   <- length(t1)
  t1      <- t1[t1len]
  x1      <- df1[df1[,3] <= 0,3]
  x1      <- x1[t1len]
  x2      <- as.numeric(df2[df2[,1] == t1,3])
  u2      <- as.numeric(df2[df2[,1] == t1,2])
  x2.safe <- hsafe(u2, leff)
  dx      <- x1 - x2
  if(dx < x2.safe) x2 <- x1 - x2.safe
  abline(v = t1, col = gray(0.8), lty = 2)
  points(t1,x2)
  # Draw trajectory
  tux1    <- df2[df2[,3] <= xfunnel,]
  tstart  <- df2[df2[,3] <= xfunnel,1]
  t1len   <- length(tstart)
  tstart  <- tstart[t1len]
  ustart  <- df2[df2[,3] <= xfunnel,2]
  ustart  <- ustart[t1len]
  xstart  <- df2[df2[,3] <= xfunnel,3]
  xstart  <- xstart[t1len]
  tend    <- t1
  uend    <- u2
  xend    <- x2
  points(tstart,xstart)
  tux2    <- trajectoryab3(tstart, tend, ustart, uend, xstart, xend, step)
  accel2  <- (xend-xstart-ustart*(tend-tstart))/(tend-tstart)^2
  print(accel2)
  umn     <- uend * 3600/5280
  tstart  <- tend
  rnum    <- as.numeric(dim(df2)[1])
  tend    <- df2[rnum,1]
  xstart  <- xend
  tux3    <- bmfree2(umn, usd, tstart, tend, xstart, step, FALSE)
  lines(tux3[,1],tux3[,3])
  df.fix  <- rbind(tux1,tux2,tux3)
  df13    <- df1[df1[,1] >= t1,]
  lines(df13[,1], df13[,3], col = "yellow", lty = 4, lwd = 3)
  lines(tux3[,1], tux3[,3], col = "yellow", lty = 4, lwd = 3)
  for(i in 1:1) {
    lst <- xabmerge3(df13,tux3,leff,step,k, TRUE)
    df3 <- lst[[1]]
    k   <- lst[[2]]
    if(k == 0) {
      df2.fix <- lst[[1]]
      tseq    <- df2.fix[,1]
      useq    <- df2.fix[,4]
      xseq    <- df2.fix[,5]
#      df2.fix <- data.frame(t = tseq, u = useq, x = xseq)
      break
    } else {
      dfab   <- lst[[3]]
      print(k)
      print(dfab)
      tstart <- as.numeric(dfab[1])
      tend   <- as.numeric(dfab[2])
      xstart <- as.numeric(dfab[5])
      xend   <- as.numeric(dfab[6])
      points(tstart,xstart)
      points(tend,xend)
      df2    <- df3[,c(1,4,5)]
      tseq   <- seq(tstart, tend, step)
      useq   <- df3[tstart <= df3[,1] & df3[,1] <= tend,4]
      xseq   <- df3[tstart <= df3[,1] & df3[,1] <= tend,5]
      lines(tseq, xseq, lwd = 2)
      df2.fix <- lst[[1]]
      tseq    <- df2.fix[,1]
      xseq    <- df2.fix[,5]
      lines(tseq, xseq, lwd = 2)
    }
  }
  tux3    <- df2.fix[,c(1,4,5)]
  df2.fix <- rbind(tux1,tux2[-1,],tux3[-1,])
  return(df2.fix)
}
