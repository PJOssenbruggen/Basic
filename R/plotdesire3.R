#' \code{plotdesire3} creates \code{t-x} desire-line trajectories.
#'
#' @param tuxv estimates from \code{brktrials3}, a matrix
#' @param xfunnel, location of the  vehicle, a vector
#' @usage plotdesire3(tuxv, xfunnel)
#' @export
plotdesire3 <- function(tuxv, xfunnel) {
  min.    <- min(as.numeric(unlist(tuxv)), na.rm = TRUE)
  max.    <- max(as.numeric(unlist(tuxv)), na.rm = TRUE)
  ylim    <- c(min., max.)
  plot(tuxv[,1],tuxv[,3], typ = "l", xlab = "t, seconds", ylab = expression(x[t]), ylim = ylim)
  abline(h = c(0, xfunnel), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  nveh <- (dim(tuxv)[2] - 1)/3
  rid    <- seq(1,nveh*3) + rep(1,nveh*3)
  vehid  <- matrix(rid, ncol = 3, byrow = TRUE)
  vseq   <- seq(1,nveh)
  vehid  <- cbind(vseq, vehid)
  colnames(vehid) <- c("vehicle", "u", "x", "y")
  speed  <- as.numeric(tuxv[,2])
  umn    <- mean(speed, na.rm = TRUE)*3600/5280
  usd    <- sd(speed, na.rm = TRUE)*3600/5280
  udf    <- data.frame(vehicle = 1, umn = umn, usd = usd)
  for(i in 2:nveh) {
    xcol   <- as.numeric(vehid[i,3])
    ucol   <- as.numeric(vehid[i,2])
    lines(tuxv[,1],tuxv[,xcol])
    speed  <- as.numeric(tuxv[,ucol])
    umn    <- mean(speed, na.rm = TRUE)*3600/5280
    usd    <- sd(speed, na.rm = TRUE)*3600/5280
    udf.   <- data.frame(vehicle = i, umn = umn, usd = usd)
    udf    <- rbind(udf, udf.)
  }
  title(main = "Desire-Lines")
  for(i in 1:nveh) {
    xcol   <- as.numeric(vehid[i,3])
    xrow   <- as.numeric(dim(tuxv)[1])
    x      <- as.numeric(tuxv[xrow,1])
    y      <- as.numeric(tuxv[xrow,xcol])
    text(x,y, labels = i, pos = 4)
  }
  return(udf)
}
