#' \code{plotupstream} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
#'
#' @return \code{plotupstream} returns \code{t-x} tractories of \code{nveh} vehicles at a bottleneck.
#' Wrapper for \code{vehdf}, \code{nopass} and \code{vehfix}
#' @param pick a lane number
#' @param lane, a vector
#' @param nveh, a number
#' @param df a data frame from \code{brktrials2}
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param type 2 to create a plot, a number
#' @usage plotupstream(pick, lane, nveh, df, xfunnel, leff, type)
#' @export
plotupstream <- function(pick, lane, nveh, df, xfunnel, leff, type) {
  tseq <- df[,1]
  tlen <- length(tseq)
  tend <- tseq[tlen]
  vehs <- lanedf(pick, nveh, lane)[,1]
  # determine ylim for data frame df
  xlimit <- vehdf(1, nveh, df)[,3]
  for(i in 2:nveh) {
    xlimit <- c(xlimit, vehdf(i, nveh, df)[,3])
  }
  ylim <- c(min(xlimit), max(xlimit))
  dfij <- vehdf(vehs[1], nveh, df)
  if(type == 2) {
    if(dfij[1,5] == "1" & type != 0) {
      plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
           xlim = c(0,tend), ylim = ylim)
      abline(h = c(0, xfunnel), col = gray(0.8))
      abline(v = 0, col = gray(0.8))
      text(dfij[tlen,1], dfij[tlen,3], labels = as.character(vehs[1]), pos = 1)
    } else {
      plot(dfij[,1], dfij[,3], xlab = "t", ylab = "x", typ = "l",
           xlim = c(0,tend), ylim = ylim, col = "blue")
      abline(h = c(0, xfunnel), col = gray(0.8))
      abline(v = 0, col = gray(0.8))
      text(dfij[tlen,1], dfij[tlen,3], labels = as.character(vehs[1]), pos = 1)
    }
  }
  vehs <- vehs[-1]
  # plot "desire" lines
  if(type == 2) {
    for(veh in 1:length(vehs)) {
      dfij <- vehdf(vehs[veh], nveh, df)
      x    <- as.numeric(dfij[,1])
      y    <- as.numeric(dfij[,3])
      lines(x, y, lty = 4)
      if(dfij[1,5] == "1") {
        lines(x, y, lty = 4)
      } else {
        lines(x, y, lty = 4, lwd = 2, col = "blue")
      }
      text(dfij[tlen,1], dfij[tlen,3], labels = as.character(vehs[veh]), pos = 1)
    }
  }
  vehs       <- lanedf(pick, nveh, lane)[,1]
  vlen       <- length(vehs) - 1
  for(veh in 1:vlen) {
    tuxlead  <- vehdf(veh = vehs[veh], nveh, df)
    tux      <- vehdf(veh = vehs[veh+1], nveh, df)
    nope     <- cbind(tuxlead[,c(1,2,3)], tux[,c(2,3)])
    colnames(nope) <- c("t", "u.lead","x.lead","u.follow","x.follow")
    tuxfix   <- nopass(veh = vehs[veh+1], nope, leff)
    if(type == 2 & dfij[1,5] == "1" & type != 0) {
      lines(tseq, tuxfix[,2])
    } else {
      if(type != 0) lines(tseq, tuxfix[,2], col = "blue")
    }
    ufix <- tuxfix[,1]
    xfix <- tuxfix[,2]
    yfix <- dfij[,4]
    df   <- vehfix(veh = vehs[veh+1], nveh, ufix, xfix, yfix, df)
  }
  if(pick != 0 & type != 0) legend("topleft",
         legend = c("No CF constraints","CF constraints"),
         lty = c(4,1), bty = "n")
  if(pick == 1 & type != 0) title(main = "Lane 1") else {
    if(type != 0)  title(main = "Lane 2")
  }
  return(df)
}
