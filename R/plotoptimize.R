#' \code{plotoptimise} produces \code{t-x} trajectories for \code{nveh} drivers
#'
#' @return \code{plotoptimize} returns  \code{t-x} tractories for \code{nveh} drivers,
#' who wish to optimize their individual desires without constraint.
#' @param df a data frame from \code{brktrials2}
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param type 2 to create a plot, a number
#' @usage plotoptimize(df,xfunnel, type)
#' @export
plotoptimize <- function(df,xfunnel,type) {
  tseq <- df[,1]
  nveh <- dim(df)[2]/6
  tlen <- length(tseq)
  # determine ylim
  xlimit <- vehdf(1, nveh, df)[,3]
  for(i in 2:nveh) {
    xlimit <- c(xlimit, vehdf(i, nveh, df)[,3])
  }
  ylim <- c(min(xlimit), max(xlimit))
  # plot
  dfij <- vehdf(1, nveh, df)
  if(type == 2) {
    if(dfij[1,5] == "1") {
      plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
           xlim = c(0,tseq[tlen]), ylim = ylim)
    } else {
      plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
           xlim = c(0,tseq[tlen]), ylim = ylim, col = "blue")
    }
    abline(h = c(0, xfunnel), col = gray(0.8))
    abline(v = 0, col = gray(0.8))
    text(dfij[tlen,1], dfij[tlen,3], labels = "1", pos = 4)
    for(veh in 2:nveh) {
      dfij <- vehdf(veh, nveh, df)
      if(dfij[1,5] == "1") {
        lines(dfij[,1], dfij[,3], lty = 4)
      } else {
        lines(dfij[,1], dfij[,3], lty = 4, lwd = 2, col = "blue")
      }
      text(dfij[tlen,1], dfij[tlen,3], labels = veh, pos = 4)
    }
    title("Car Following Constraints")
    legend("topleft", legend = c(
      "Leading vehicle",
      "Following vehicles",
      "Lane 1", "Lane 2"
    ),
    lty = c(1,4,1,1),
    col = c("black","black","black","blue"),
    bty = "n")
  }
  return(df)
}
