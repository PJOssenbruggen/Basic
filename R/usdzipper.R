#' The \code{usdzipper} function is used to perform a sensitivity analysis of a single vehicles
#' in traffic breakdown.
#'
#' @return \code{usdzipper} uses a deterministic model to illustrate an ``idealistic'' situtaion,
#' a so-called a ``usdzipper merge.''
#' @param tstart start time, a number
#' @param tend end time, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a number
#' @param xend end location for vehicle in lane 1 (feet), a number
#' @usage usdzipper(tstart, tend, umn, usd, xstart, xend)
# #' @examples
# #' usdzipper(0, 5, 41, 11.6, 0, 500)
#' @export
usdzipper  <- function(tstart, tend, umn, usd, xstart, xend)
{
  mysample <- rnorm(1000, umn, usd)
#  hist(mysample, col = gray(0.8))
  ustart <- sample(mysample[mysample > 50], 1)
  uend  <- sample(mysample[mysample <= 50], 1)
  step  <- tend/1000
  tseq  <- seq(0, tend, by = step)
  u0    <- ustart * 5280/3600
  uend  <- uend * 5280/3600
  x0    <- xstart
  t0    <- 0
  usd   <- usd * 5280/3600
  xfseq  <- ufseq <- xlseq <- ulseq <- {}
  dfab   <- xabparam(tstart, tend, ustart = u0, uend = uend, xstart = x0, xend = xend)
  a1     <- dfab[1]
  b1     <- dfab[2]
  W      <- usd * sqrt(step) * rnorm(length(tseq),0,1)
  df1    <- data.frame(t = tseq,
                    u = uab(u0, a = a1, b = b1, tseq, t0),
                    x = xab(x0, u0,a = a1, b = b1, tseq, t0),
                    W,
                    uvol = rep(NA, length(tseq)),
                    xvol = rep(NA, length(tseq)),
                    u0 = rep(u0, length(tseq)),
                    uend = rep(uend, length(tseq)),
                    a = rep(a1, length(tseq)),
                    b = rep(b1, length(tseq))
                    )
  df1[1,5] <- df1[1,2] + df1[1,4]
  df1[1,6] <- x0
  for(i in 2:length(tseq)) {
    df1[i,5]  <- df1[i-1,2] + df1[i,4]
    df1[i,6]  <- df1[i-1,3] + df1[i,5] * step
  }
  ustart <- sample(mysample[mysample > 50], 1)
  uend  <- sample(mysample[mysample <= 50], 1)
  u0    <- ustart * 5280/3600
  uend  <- uend * 5280/3600
  x0    <- -1 * hsafe(3600/5250*ustart, 14)
  xend   <- xend - hsafe(3600/5250*uend, 14)
  dfab   <- xabparam(tstart, tend, ustart = u0, uend = uend, xstart = x0, xend = xend)
  a1     <- dfab[1]
  b1     <- dfab[2]
  df3    <- data.frame(t = tseq,
                      u = uab(u0, a = a1, b = b1, tseq, t0),
                      x = xab(x0, u0,a = a1, b = b1, tseq, t0),
                      W,
                      uvol = rep(NA, length(tseq)),
                      xvol = rep(NA, length(tseq)),
                      u0 = rep(u0, length(tseq)),
                      uend = rep(uend, length(tseq)),
                      a = rep(a1, length(tseq)),
                      b = rep(b1, length(tseq))
                      )
  df3[1,5] <- df3[1,2] + df3[1,4]
  df3[1,6] <- x0
  for(i in 2:length(tseq)) {
    df3[i,5]  <- df3[i-1,2] + df3[i,4]
    df3[i,6]  <- df3[i-1,3] + df3[i,5] * step
  }
  par(mfrow = c(1,2), pty = "s")
  plot(df1[,1],df1[,2],typ = "l", xlab = "t, seconds", ylab = expression(u[t]*", fps"),ylim=c(0,150))
  lines(df1[,1],df1[,5], col = "blue")
  lines(df3[,1],df3[,5], col = "red")
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  abline(h = 50*5280/3600, lty = 3)
  axis(side = 4, at = 50*5280/3600, labels = expression(u*"*"))
  plot(df1[,1],df1[,3],typ = "l", xlab = "t, seconds", ylim = c(-100,600),
       ylab = expression(x[t]*", feet"))
  lines(df1[,1],df1[,6], col = "blue")
  lines(df3[,1],df3[,6], col = "red")
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  legend("topleft", legend = c("Vehicle 1", "Vehicle 2"),
         lty = c(1,1), col = c("blue", "red"), bty = "n")
  par(mfrow = c(1,1), pty = "s")
  df2mn = round(apply(df1, 2, mean),1)
  df2sd = round(apply(df1, 2, sd),1)
  df2 = rbind(df2mn,df2sd)
  rownames(df2) = c("mean", "sd")
  df2 <- df2[,c(2,4,7,8,9,10)]
  df3mn = round(apply(df3, 2, mean),1)
  df3sd = round(apply(df3, 2, sd),1)
  df3 = rbind(df3mn,df3sd)
  rownames(df3) = c("mean", "sd")
  df3 <- df3[,c(2,4,7,8,9,10)]
  return(list(df2, df3))
}

