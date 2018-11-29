#' \code{bmfree2} returns a \code{T.} matrix of time \code{t}, speed \code{u} (fps)
#'  and location \code{x} (feet) using a Brownian Motion Bridge model for one vehicle..
#'
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{usd} (mph), a number
#' @param tstart simulation start time, a number
#' @param tend simulation end time, a number
#' @param xstart location (feet), a number
#' @param browse logical for plotting where TRUE creates a plot
#' @param step time-step size, a number
#' @usage bmfree2(umn, usd, tstart, tend, xstart, step, browse)
# #' @examples
# #' bmfree2(41, 11, 16.5, 30, 10, 0.25, TRUE)
#' @export
bmfree2  <- function(umn, usd, tstart, tend, xstart, step, browse) {
  set.seed(333)
  t     <- seq(tstart, tend, step)
  N     <- length(t)
  W     <- numeric(N)
  usd.  <- usd
  usd   <- 5280/3600*usd
  for(i in 2:(N)) W[i] <- W[i-1] + usd * sqrt(step) * rnorm(1)
  x     <- 5280/3600*umn
  y     <- 5280/3600*umn
  u     <- x + (W - (t - tstart)/(tend - tstart) * (W[N] - y + x))
  for(i in 1:N) if(u[i] <= 0) u[i] = 0
  x[1]  <- xstart
  for(i in 2:N) x[i] <- x[i-1] + step * u[i-1]
  T.    <- as.matrix(data.frame(t, u, x))
  if(browse == TRUE) {
    pdf(file = "/Users/PJO/Desktop/SVM.pdf")
    par(mfrow = c(1,2), pty = "s")
    plot(T.[,1], T.[,2], typ = "l", ylab = "u, feet per second", xlab = "t, seconds",lwd = 2,
         col = "blue", ylim=c(0,120))
    abline(h = 0, col = gray(0.8))
    abline(v = c(0,tend), col = gray(0.8))
    axis(side = 4, at = 5280/3600*umn, labels = "u", lty = 2, line = -1,tick=FALSE)
    abline(h = 5280/3600*umn, lty = 2)
    #lines(T.[,1], x, col = "red", lwd = 2)
    legend("topleft",
           #  title = "",
           legend = c(
             expression(""),
             bquote(bar(u) == .(umn)),
             bquote(sigma[U] == .(usd.))
           ),
           cex = c(0.75,0.75))
    title(main = "Single-Vehicle Motion Model", sub = "Lead Vehicle")
    plot(T.[,1], T.[,3], typ = "l", ylab = "x, feet", xlab = "t, seconds", lwd = 2,
         col = "blue", ylim=c(-1000,2000))
    abline(h = c(0,-500), col = gray(0.8))
    abline(v = c(0,tend), col = gray(0.8))
    lines(c(0,tend),c(xstart,xstart+5280/3600*umn*tend),lty=2)
    axis(side = 4, at = -500, labels = expression(x[e]), lty = 2, line = -1,tick=FALSE)
    axis(side = 4, at = 0, labels = expression(x[0]), lty = 2, line = -1,tick=FALSE)
    t1    <- max(T.[T.[,3]<=0,1])
    t2    <- max(T.[T.[,3]<=-500,1])
    abline(v = c(t1,t2),lty = 3)
    axis(side = 3, at = t1, labels = expression(T[1]), lty = 3, line = -1,tick=TRUE)
    axis(side = 3, at = t2, labels = expression(T[2]), lty = 3, line = -1,tick=FALSE)

    title(main = "Desire-Lines", sub = "Zipper Merge")
    # A second draw is made for a second vehicle and plotted.
    t     <- seq(tstart, tend, step)
    N     <- length(t)
    W     <- numeric(N)
    usd   <- 5280/3600*usd
    for(i in 2:(N)) W[i] <- W[i-1] + usd * sqrt(step) * rnorm(1)
    x     <- 5280/3600*umn
    y     <- 5280/3600*umn
    u     <- x + (W - (t - tstart)/(tend - tstart) * (W[N] - y + x))
    for(i in 1:N) if(u[i] <= 0) u[i] = 0
    x     <- rep(NA,N)
    x[1]  <- -800
    for(i in 2:N) x[i] <- x[i-1] + step * u[i-1]
    lines(T.[,1], x, col = "red", lwd = 2)
    legend("topleft", legend = c("Lead vehicle","Following vehicle"),
           lwd = c(2,2), col = c("blue","red"), bty = "n")
    dev.off()
  }
  # if(browse == TRUE) browser()
  # return T. for the lead vehicle.
  return(T.)
}
