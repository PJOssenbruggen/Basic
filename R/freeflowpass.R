#' The \code{freeflowpass} function estimates the relative locations of two vehicles where one passes the other.
#'
#' @return \code{freeflowpass} uses a deterministic model to show the locations of vehicle accelerating to pass
#' another vehicle traveling side-by-side at the same speed.
#' @param tstart start time, a number
#' @param tend end time, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location for vehicle in lane 1 (feet), a number
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @usage freeflowpass(tstart, tend, umn, usd, xstart, xfunnel, leff)
#' @export
freeflowpass  <- function(tstart, tend, umn, usd, xstart, xfunnel, leff) {
  x1 <- x2 <- {}
  y1 <- y2 <- {}
  delt  <- tend/10
  tseq  <- seq(0, 5*tend, by = delt)
  usd   <- usd * 5280/3600
  W     <- usd * sqrt(delt) * rnorm(length(tseq),0,1)
  x1    <- xstart + umn *5280/3600 * tseq
  for(i in 1:length(x1)) x1[i]    <- x1[i] + W[i]
  y1    <- rep(NA, length(tseq))
  for(i in 1:length(x1)) {
    if(x1[i] > 0) y1[i] = 0
    if(x1[i] <= 0 & x1[i] >= xfunnel) y1[i] = 6/xfunnel * abs(x1[i])
    if(x1[i] < xfunnel) y1[i] = -6
  }
  df1      <- data.frame(t = tseq, u1 = rep(umn*5280/3600, length(tseq)), x1, y1)
  lane     <- rep(1, dim(df1)[1])
  df1      <- cbind(df1, lane)
  colnames(df1) <- c("t", "u", "x", "y", "lane")
  # Lane 2
  tseq.   <- tseq[x1 <= -500]
  x2.     <- xstart + umn *5280/3600 * tseq.
  df2     <- data.frame(t = tseq., u2 = rep(umn*5280/3600), x2 = x2., y2 = rep(6,length(tseq.)))
  tstart2 <- max(tseq[x1 <= -500])
  tend2   <- max(tseq[x1 <= 0])
  tseq2   <- seq(tstart2, tend2, delt)
  xstart2 <- max(x2.)
  ustart2 <- uend2 <- umn*5280/3600
  xstart2 <- max(x2.)
  xend2   <- x1[tseq == tend2] + hsafe(uend2, leff)
  df2bk   <- data.frame(tstart2,tend2,ustart2,uend2,xstart2,xend2)
  ab      <- xabparam(tstart2, tend2, ustart2, uend2, xstart2, xend2)
  tseq3   <- seq(tstart2, tend2, delt)
  x2..    <- xab(xstart2, ustart2,  ab[1], ab[2], tseq3, tstart2)
  u2..    <- uab(ustart2,  ab[1], ab[2], tseq3, tstart2)
  y2..    <- rep(NA, length(x2..))
  for(i in 1:length(x2..)) y2..[i] = -6/xfunnel * abs(x2..[i])
  df2..   <- data.frame(t = tseq3, u2 = u2.., x2 = x2.., y2 = y2..)
  x2      <- c(x2., x2..[-1])
  df2     <- rbind(df2, df2..[-1,])
  tstart3 <- tend2
  tend3   <- 5*tend
  tseq3   <- seq(tstart3, tend3, delt)
  x2..    <- max(x2..) + umn *5280/3600 * (tseq3[-1] - min(tseq3))
  x2      <- c(x2, x2..[-1])
  df2...  <- data.frame(t = tseq3[-1], u2 = rep(umn *5280/3600 ,length(tseq3[-1])), x2 = x2.., y2 = rep(NA, length(tseq3[-1])))
  df2     <- rbind(df2, df2...)
  for(i in 1:dim(df2)[1]) {
    if(df2[i,3] > 0) df2[i,4] = 0
    if(df2[i,3] <= 0 & df2[i,3] >= xfunnel) df2[i,4] = -6/xfunnel * abs(df2[i,3])
    if(df2[i,3] < xfunnel) df2[i,4] = 6
  }
  lane     <- rep(2, dim(df2)[1])
  df2      <- cbind(df2, lane)
  colnames(df2) <- c("t", "u", "x", "y", "lane")
  df       <- rbind(df1[1:25,], df2[1:25,])
  df[,5]   <- factor(df[,5])
#  plot(y1[x1 > -700 & x1 < 200], x1[x1 > -700 & x1 < 200], pch = 16, cex = 0.75, ylim = c(-700, 200),
#       xlim = c(-7,7),      col = "green", axes = FALSE, ylab = "", xlab = "")
#  y2 <- df2[,4]
#  x2 <- df2[,3]
#  points(y2[x2 > -700 & x2 < 200], x2[x2 > -700 & x2 < 200], pch = 16, cex = 0.75, col = "red")
#  text(3,0, expression("x = 0"))
#  text(0,-500, expression("x = -500"))
#  abline(v = 0, col = gray(0.8))
#  abline(h = c(0, -500), col = gray(0.8))
#  title("Free-Flow Pass and Merge")
#  legend("topleft", legend = c("Lane 1", "Lane 2"),
#         col = c("green", "red"),
#         pch = c(16,16),
#         bty = "n")
  return(df)
}
