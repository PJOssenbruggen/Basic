#' \code{plotmerge3} creates \code{t-x} safe headway trajectories.
#'
#' @param tuxv.fix estimates from \code{brktrials3}, a matrix
#' @param vehorder vehicle arrivals at \code{x} = 0, a vector
#' @param xfunnel, location of the  vehicle, a vector
#' @usage plotmerge3(tuxv.fix, vehorder, xfunnel)
#' @export
plotmerge3 <- function(tuxv.fix, vehorder, xfunnel) {
  min.    <- min(as.numeric(unlist(tuxv.fix)), na.rm = TRUE)
  max.    <- max(as.numeric(unlist(tuxv.fix)), na.rm = TRUE)
  ylim    <- c(min., max.)
  plot(tuxv.fix[,1],tuxv.fix[,3], typ = "l", xlab = "t, seconds", ylab = expression(x[t]), ylim = ylim)
  abline(h = c(0, xfunnel), col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  nveh   <- (dim(tuxv.fix)[2] - 1)/3
  rid    <- seq(1,nveh*3) + rep(1,nveh*3)
  vehid  <- matrix(rid, ncol = 3, byrow = TRUE)
  vseq   <- seq(1,nveh)
  vehid  <- cbind(vseq, vehid)
  colnames(vehid) <- c("vehicle", "u", "x", "y")
  speed  <- as.numeric(tuxv.fix[,2])
  uall   <- speed
  umn    <- mean(speed, na.rm = TRUE)*3600/5280
  usd    <- sd(speed, na.rm = TRUE)*3600/5280
  udf    <- data.frame(vehicle = 1, umn = umn, usd = usd)
  for(i in 2:nveh) {
    xcol   <- as.numeric(vehid[i,3])
    ucol   <- as.numeric(vehid[i,2])
    lines(tuxv.fix[,1],tuxv.fix[,xcol])
    speed  <- as.numeric(tuxv.fix[,ucol])
    uall   <- c(uall, speed)
    umn    <- mean(speed, na.rm = TRUE)*3600/5280
    usd    <- sd(speed, na.rm = TRUE)*3600/5280
    udf.   <- data.frame(vehicle = i, umn = umn, usd = usd)
    udf    <- rbind(udf, udf.)
  }
  title(main = "Bottleneck Merge")
  for(i in 1:nveh) {
    xcol   <- as.numeric(vehid[vehorder[i],3])
    xrow   <- as.numeric(dim(tuxv.fix)[1])
    x      <- as.numeric(tuxv.fix[xrow,1])
    y      <- as.numeric(tuxv.fix[xrow,xcol])
    text(x,y, labels = vehorder[i], pos = 4)
  }
  u.all    <- mean(uall, na.rm = TRUE)*3600/5280
  sd.all   <- sd(uall, na.rm = TRUE)*3600/5280
  udf.     <- data.frame(vehicle = nveh, umn = u.all, usd = sd.all)
  udf      <- rbind(udf, udf.)
  return(udf)
}
