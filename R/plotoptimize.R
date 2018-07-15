#' \code{plotoptimise} produces \code{t-x} trajectories for \code{nveh} drivers
#'
#' @return \code{plotoptimize} returns  \code{t-x} tractories for \code{nveh} drivers,
#' who wish to optimize their individual desires without constraint.
#' @param df a data frame from \code{brktrials2}
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @usage plotoptimize(df,xfunnel)
#' @export
plotoptimize <- function(df,xfunnel) {
  tseq <- df[,1]
  nveh <- dim(df)[2]/6
  tlen <- length(tseq)
  # determine tlim
  xlimit <- vehdf(1, nveh, df)[,3]
  for(i in 2:nveh) {
    xlimit <- c(xlimit, vehdf(i, nveh, df)[,3])
  }
  ylim <- c(min(xlimit), max(xlimit))
  # plot
  dfij <- vehdf(1, nveh, df)
  plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
       xlim = c(0,tseq[tlen]), ylim = ylim)
  abline(h = c(0, xfunnel), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  text(dfij[tlen,1], dfij[tlen,3], labels = "1", pos = 4)
  for(veh in 2:nveh) {
    dfij <- vehdf(veh, nveh, df)
    lines(dfij[,1], dfij[,3], lty = 4)
    text(dfij[tlen,1], dfij[tlen,3], labels = veh, pos = 4)
  }
  legend("topleft", legend = c("Leading vehicle","Following vehicles"),
         lty = c(1,4), bty = "n")
}
