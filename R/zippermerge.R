#' \code{zippermerge} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
#'
#' @return The analyses the zone for a safe headway violation. \code{X} = 0 if safe, else non-zero.
#' @param nveh number of vehicles, a number
#' @param tstart start time, (seconds), a number
#' @param tend end time, (seconds), a number
#' @param xstart vehicle location at time \code{tstart}, a number
#' @param u vehicle speed (mph), a number
#' @param xfunnel location where the lane drop is located, a number
#' @param leff vehicle length in feet, a number
#' @param step size in seconds, a number
#' @param type TRUE to create plots or FALSE otherwise, a logical
#' @usage zippermerge(nveh, tstart, tend, xstart, u, leff, xfunnel, step, type)
# #' @examples
# #' zippermerge(nveh, 0, 1.5, -700,53.1, leff, xfunnel, step, TRUE)
#' @export
zippermerge <- function(nveh, tstart, tend, xstart, u, leff, xfunnel, step, type) {
  tseq <- seq(tstart, tend, step)
  tlen <- length(tseq)
  k    <- round(as.numeric(5280/hsafe(u*5280/3600,leff)))
  x    <- matrix(rep(NA,tlen*nveh), ncol = nveh)
  colnames(x) <- paste("x",sep="",1:nveh)
  s    <- c(0,cumsum(rep(5280/k,nveh-1)))
  u    <- 5280/3600*u
  for(veh in 1:nveh) {
    for(i in 1:tlen) {
      x[i, veh] <- xstart - s[veh] + u * (tseq[i] - tstart)
    }
  }
  u <- rep(u,tlen)
  y <- rep(0,tlen)
  df1df2 <- as.matrix(data.frame(t = tseq, u, x = x[,1], y))
  for(veh in 2:nveh) {
    df1df2. <- as.matrix(data.frame(u, x = x[,veh], y))
    df1df2  <- cbind(df1df2, df1df2.)
  }
  if(type == TRUE) {
    ylim <- c(min(x),max(x))
    par(mfrow = c(1,1), pty = "s")
    plot(tseq, x[,1], type = "l", xlab = "t, seconds", lwd = 2,
         ylab = "x, feet", ylim, xlim = c(0,tend),col = "blue")
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(x[,1]), labels = 1,pos=4,cex=0.5, offset = 0.2,lwd = 2)
    text(0, min(x[,1]), labels = 1,pos=2,cex=0.5, offset = 0.2,lwd = 2)
    title(main = "Bottleneck", sub = "A zipper merge.")
    axis(side = 3, at = tend/2, "Deterministic Model", tick = FALSE, line = -1)
    vblue <- seq(1,nveh,2)
    vred  <- seq(2,nveh,2)
    for(veh in 2:nveh) {
      if(any(vred == veh)) {
        lines(tseq,x[,veh], col = "red", lty = 1, lwd = 2)
        text(tend, max(x[,veh]), labels = veh, pos=4, cex=0.5, offset = 0.2, lwd = 2)
        text(0, min(x[,veh]), labels = veh, pos=2, cex=0.5, offset = 0.2, lwd = 2)
      } else {
        lines(tseq,x[,veh], col = "blue", lty = 1, lwd = 2)
        text(tend, max(x[,veh]), labels = veh, pos=4, cex=0.5, offset = 0.2, lwd = 2)
        text(0, min(x[,veh]), labels = veh, pos=2, cex=0.5, offset = 0.2, lwd = 2)
      }
    }
    h       <- {}
    for(veh in 2:nveh) {
      hd <- max(tseq[x[,veh] <= 0]) - max(tseq[x[,veh-1] <= 0])
      h  <- c(h,hd)
    }
    flow    <- round(3600/mean(h),0)
    u.mph   <- 3600/5280 * u
    browser()
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    legend("topleft", legend = c(
      title = "",
      expression("Initial conditions:"),
      bquote(u[0] == .(umn)),
      bquote(sigma[U] == .(usd)),
      bquote(k[0] == .(k))),
      cex = c(0.75,0.75,0.75))
  }
  return(df1df2)
}
