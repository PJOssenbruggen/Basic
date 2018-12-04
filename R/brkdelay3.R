#' \code{brkdelay3} is a wrapper function for \code{brktrials3}.
#'
#' @return \code{brkdelay3} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
#' @param niter number of iterations, a number
#' @param nveh number of vehicles, a number
#' @param tstart start time for a simulation run, a number
#' @param tend end time for a simulation run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart start location, (feet), a number
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param delt size in seconds, a number
#' @usage brkdelay3(niter, nveh, tstart, tend, umn, usd, xstart, xfunnel, leff, delt)
# #' @examples
# #' brkdelay3(10, 4, 0, 30, 68.8, 4.4, -700, -500, 14,  0.25)
#' @export
brkdelay3 <- function(niter, nveh, tstart, tend, umn, usd, xstart, xfunnel, leff, delt) {
  type <- FALSE
  df   <- brktrials3(nveh, umn, usd, tstart, tend, xstart, delt, type, leff, xfunnel)[[2]]
  tseq <- df[,1]
  udf  <- df[,seq(2,dim(df)[2],6)]
  u    <- rowMeans(udf)
  df.  <- cbind(t = tseq, u)
  for(i in 2:niter) {
    df   <- brktrials3(nveh, umn, usd, tstart, tend, xstart, delt, type, leff, xfunnel)[[2]]
    udf  <- df[,seq(2,dim(df)[2],6)]
    u    <- rowMeans(udf)
    df.  <- cbind(df., u)
  }
  umn <- apply(df.[,-1],1,mean)*3600/5280
  usd <- apply(df.[,-1],1,sd)*3600/5280
  plot(tseq, umn, typ = "p", pch = 16, ylim = c(0,90),
       xlab = "t, seconds", ylab = expression(u[t]*", mph"))
  abline(h = c(0,umn[1],50), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  axis(side = 4, at = 50, labels = expression(u^"*"))
  for(i in 1:length(tseq)) {
    lines(x = c(tseq[i],tseq[i]), y = c(umn[i] - usd[i], umn[i] + usd[i]))
  }
  title("Bottleneck Delay")
  legend("topright", legend = expression(bar(u) %+-% sigma[U]),
         pch = 16,
         lty = 1,
         bty = "n"
  )
}
