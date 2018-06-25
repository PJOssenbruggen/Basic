#' \code{rrtrials} produces \code{t-x} trajectories for lead and following vehicles on a ring road
#'
#' @return The \code{rrtrials} function returns a x-tractories or \code{t-x} plots
#' for the lead and following vehicles on a single-lane road. This is a strict car-following
#' model. Crossing trajectories are disallowed. In addition,
#' a following vehicle driver maintains a safe headway while attempting to minimize
#' travel time. In other words, the following vehicle's movement is constrained. Output
#' from The \code{rrtrials} is an output matrix called \code{LF}.
#' @param umn lead vehicle speed (mph), a number
#' @param usd standard deviation of lead vehicle speed (mph), a number
#' @param k0 traffic density (vpm), a number
#' @param N number of time-steps, a number
#' @param T upper time range (seconds), a number
#' @param leff effective vehicle length (feet), a number
#' @param nveh number of following vehicles, a number
#' @param xlim plot range along the \code{t} axis, a vector
#' @param ylim plot range along the \code{x} axis, a vector
#' @usage rrtrials(umn, usd, k0, N, T, leff, nveh, xlim, ylim)
# #' @examples
# #' rrtrials(41, 11, 50, 120, 120, 14, 9,   c(0, 120), c(-1000, 7500))
# #' rrtrials(2,  2, 55, 60, 60, 14,    9,   c(0, 60), c(-2000, 600))
# #' rrtrials(18.8, 3.8, 55, 120, 120, 14, 9, c(0, 120), c(-1000, 2000))
#' @export
rrtrials <- function(umn, usd, k0, N, T, leff, nveh, xlim, ylim) {
  t0    <- 0
  input <- as.matrix(data.frame(umn, usd, k0, N, T, leff, nveh))
  lead  <- bmfree(umn, usd, N, T)
  foll  <- bmfree(umn, usd, N, T)
  u     <- c(lead[,2], foll[,2])
  nobs  <- dim(lead)[1]
  h0    <- 5280/k0
  foll[,3]  <- foll[,3] - rep(h0, nobs)
  h     <- lead[,3] - foll[,3]
  tseq  <- lead[,1]
  xf    <- rep(NA, nobs)
  uf    <- rep(NA, nobs)
  hsf   <- rep(NA, nobs)
  for(i in 1:nobs) hsf[i] <- hsafe(foll[i,2], leff)
  xl    <- lead[,3]
# headway analysis: If safe, no adjustments are made to spacing or speed of the following vehicle are made.
  for(i in 1:nobs) if(h[i] >= hsf[i]) xf[i] <- foll[i,3] else xf[i] <- lead[i,3] - hsf[i]
  for(i in 1:nobs) if(h[i] >= hsf[i]) uf[i] <- foll[i,2] else uf[i] <- lead[i,2]
  x <- c(xl, xf)
#  ylimx = c(min(x),max(x))
  plot(tseq, xl, typ = "l", xlab = "t, seconds", ylab = expression(x[t]*", feet"),
       xlim = xlim, ylim = ylim, lwd = 1)
  lines(c(0,T), c(0, 5280/3600*umn * T), col = gray(0.5), lty = 3)
  lines(tseq, xf,lwd = 1)
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  mtext(text = bquote(bar(u) == .(umn)), line = 1)
  mtext(text = bquote(hat(sigma) == .(usd)), line = 0)
# curve 2, etc
  lines(tseq, foll[,3], lty = 2, col = gray(0.5))
  lines(tseq, xf, lty = 1, lwd = 1, col = gray(0))
  LF      <- cbind(lead[,c(1,3)], xf)
  LFu     <- cbind(lead[,c(1,2)], uf)
# curves k = 3,4,...
  for(k in 2:nveh) {
    if(k == 4 | k == 5)
    lead     <- foll
    lead[,3] <- xf
    foll     <- bmfree(umn, usd, N, T)
    foll[,3] <- foll[,3] - rep(h0, nobs) * k
    h        <- lead[,3] - foll[,3]
    for(i in 1:nobs) hsf[i] <- hsafe(foll[i,2], leff)
    for(i in 1:nobs) if(h[i] >= hsf[i]) xf[i] <- foll[i,3] else xf[i] <- lead[i,3] - hsf[i]
    for(i in 1:nobs) if(h[i] >= hsf[i]) uf[i] <- foll[i,2] else uf[i] <- lead[i,2]
    lines(tseq, xf, lty = 1, lwd = 1, col = gray(0))
    LF   <- as.matrix(data.frame(LF, xf))
    colnames(LF) <- c("t", paste(1:(k+1)))
    LFu  <- as.matrix(data.frame(LFu, uf))
    colnames(LFu) <- c("t", paste(1:(k+1)))
  }
# t5 is the time the vehicle crosses x = 0
  dt  <- N/T
  t5  <- rrheadway(LF, dt)
  k   <- 0
  for(i in 1:length(t5)) if(!is.na(t5[i])) k <- k + 1
  points(t5[1:k], c(rep(0,k)), pch = 16, cex = 0.5)
  t5  <- t5[1:k]
  LFu <- LFu[,1:(k + 1)]
  u5  <- rrheadwayu(LFu, t5)
# Summary
  t4      <- xlim[2]
  abline(v = t4, col = gray(0.8))
  axis(side = 3, at = c(t0, t4), labels = c(expression(t[0]), expression(t[4])))
  ncase    <- nveh + 1
  text(rep(0,ncase), c(LF[1,2:dim(LF)[2]]), labels = seq(1,ncase),
       cex = 0.5, pos = 2, offset = 0.4)
  text(rep(t4,ncase), c(LF[dim(LF)[1],2:dim(LF)[2]]), labels = seq(1,ncase),
       cex = 0.5, pos = 4, offset = 0.4)
  hdwy    <- c(t5[1], diff(t5))
  hdwy.mn <- mean(hdwy)
  hdwy.sd <- sd(hdwy)
  q       <- 3600 / hdwy.mn
  lengthu5<- 5280 * nveh / k0
  tden    <- 5280 * nveh / lengthu5
# u5 measured in mph
  u5      <- u5 * 3600 / 5280
  u5.mn   <- mean(u5)
  u5.sd   <- sd(u5)
  flow5   <- 5280 * sum(u5) / lengthu5
  exp.flow<- 5250 * nveh * umn / lengthu5
  expect <- as.matrix(data.frame(mph = umn,
                   density.vpm = round(tden,0),
                   flow.vph = round(exp.flow,0),
                   section.length = round(lengthu5,0))
        )
  headway5 <- as.matrix(data.frame(hdwy.mean = round(hdwy.mn, 2),
                   hdwy.sd = round(hdwy.sd,2),
                   flow.vph.headway = round(q,0))
        )
  speed5 <- as.matrix(data.frame(mean.mph = round(u5.mn,1),
                   u5.sd.mph = round(u5.sd,1),
                   flow.vph.speed = round(flow5,0),
                   sample.size = k)
        )
  output = list(input, LF, LFu, expect, headway5, speed5, hdwy)
  return(output)
}
