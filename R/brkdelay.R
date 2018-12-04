#' \code{brkdelay} is a wrapper function for \code{brktrials2}.
#'
#' @return \code{brkdelay} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
#' @param niter number of iterations, a number
#' @param tend end time for a simulation run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart a vector of  \code{nveh} start locations, (feet), a vector
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane a vector of \code{nveh} numbers, a vector
#' @param delt size in seconds, a number
#' @usage brkdelay(niter, tend, umn, usd, xstart, xfunnel, leff, lane, delt)
# #' @examples
# #' brkdelay(2, 30, 41, 11, xstart, -500, 14, lane, 0.5)
#' @export
brkdelay <- function(niter, tend, umn, usd, xstart, xfunnel, leff, lane, delt) {
  df   <- brktrials2(tend, umn, usd, xstart, xfunnel, leff, lane, delt, type = 0)[[2]]
  tseq <- df[,1]
  udf  <- df[,seq(2,dim(df)[2],6)]
  u    <- rowMeans(udf)
  df.  <- cbind(t = tseq, u)

  for(i in 2:niter) {
    df   <- brktrials2(tend, umn, usd, xstart, xfunnel, leff, lane, delt, type = 0)[[2]]
    udf  <- df[,seq(2,dim(df)[2],6)]
    u    <- rowMeans(udf)
    df.  <- cbind(df., u)
  }
  umn <- apply(df.[,-1],1,mean)*3600/5280
  usd <- apply(df.[,-1],1,sd)*3600/5280
  plot(tseq, umn, typ = "p", ylim = c(0,70), pch = 16,
       xlab = "t, seconds", ylab = expression(u[t]*", mph"))
  abline(h = c(0,umn[1]), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
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
