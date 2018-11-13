#' \code{idealtrajectory} creates a \code{t-x} trajectory for a deterministic model.
#'
#' @return \code{idealtrajectory} returns the mean and standard deviation of a idealtrajectory estimate.
#' @param df1df2 leading vehicle, a matrix
#' @param nveh number of vehicles, a number
#' @param u speed in mph, a number
#' @param k density in vpm, a number
#' @usage idealtrajectory(df1df2, nveh, u, k)
# #' @examples
# #' idealtrajectory(6, 50.4, 85,)
#' @export
idealtrajectory <- function(df1df2, nveh, u, k) {
#  par(mfrow = c(1,1), pty = "s")
  tend <- 30
  plot(df1df2[,1], df1df2[,3], typ = "n", xlim = c(0,15),
       ylim = c(-850,200), ylab = "x, feet", xlab = "t, seconds")
  abline(h = c(0,-500), col = gray(0.8))
  abline(v = c(0), col = gray(0.8))
  browser()
  u      <- u * 5280/3600
  h      <- 5280/k
  tau    <- h/u
  t1     <- 700/u
  points(t1,0)
  points(t1+tau,0)
  points(t1+2*tau,0)
  points(t1+0.5*tau,0, pch = 16, cex = 0.75)
  points(t1+1.5*tau,0, pch = 16, cex = 0.75)
  points(t1+2.5*tau,0, pch = 16, cex = 0.75)
  t2 <- 200/u
  points(t2,-500, pch = 16, cex = 0.75)
  points(t2+tau,-500, pch = 16, cex = 0.75)
  points(t2+2*tau,-500, pch = 16, cex = 0.75)
  points(t2,-500, pch = 1, cex = 1.5)
  points(t2+tau,-500, pch = 1, cex = 1.5)
  points(t2+2*tau,-500, pch = 1, cex = 1.5)
  lines(c(0,tend), c(-700,-700+u*tend))
  lines(c(0,tend), c(-700 - h, -700-h + u*tend))
  lines(c(0,tend), c(-700 -2* h, -700-2*h + u*tend))
  lines(c(t2,t1+0.5*tau), c(-500,0))
  x3 <- u*(tend-(t1+0.5*tau))
  lines(c(t1+0.5*tau,tend), c(0,x3))
  x3 <- u*(tend-(t1+1.5*tau))
  lines(c(t1+1.5*tau,tend), c(0,x3))
  x3 <- u*(tend-(t1+2.5*tau))
  lines(c(t1+2.5*tau,tend), c(0,x3))
  lines(c(t2+tau,t1+1.5*tau), c(-500,0))
  lines(c(t2+2*tau,t1+2.5*tau), c(-500,0))
  axis(side = 4, at = 0, labels = expression(x[0]))
  axis(side = 4, at = -500, labels = expression(x[e]))
  title(main = "Railroad Track Merge", sub = "Signal Control")
  legend("topleft", legend = c("Lead Vehicle","Following Vehicle"),
         pch = c(1,16),  bty = "n")
  capacity <- 3600/tau
  df1df2 <- matrix(rep(NA, 4*13),ncol=13)
  colnames(df1df2) <- c("t", "u.1.1", "x.1.1", "u.2.1", "x.2.1",
                        "u.1.2", "x.1.2", "u.2.2", "x.2.2",
                        "u.1.3", "x.1.3", "u.2.3", "x.2.3")
  df1df2[1,1] <- 0
  df1df2[2,1] <- t2
  df1df2[3,1] <- t1
  df1df2[4,5] <- 0

  df1df2[,2]  <- df1df2[,4] <- as.matrix(rep(u,4))
  df1df2[3,4] <- NA
  u2 <- 500/(t1-t2+tau)
  df1df2[4,4]  <- u2
  df1df2[1,3]  <- df1df2[1,5] <- -700
  df1df2[2,3]  <- df1df2[2,5] <- -500
  df1df2[3,3]  <- 0
  df1df2[4,1]  <- df1df2[3,1] + tau/2
  df1df2[1,c(6,8,10,12)] <- u
  df1df2[1,c(7,9)] <- df1df2[1,3] - h
  df1df2[1,c(11,13)] <- df1df2[1,3] - 2*h
  df1df2 <- rbind(df1df2, df1df2)
  for(i in 5:8) df1df2[i,1] <- as.numeric(df1df2[4,1]) + tau/2 * (i-4)
  df1df2[8,13] <- 0
  df1df2[7,11] <- 0
  df1df2[6,9]  <- 0
  df1df2[5,7]  <- 0
  df1df2. <- matrix(rep(NA, 2*13),ncol=13)
  colnames(df1df2.) <- c("t", "u.1.1", "x.1.1", "u.2.1", "x.2.1",
                        "u.1.2", "x.1.2", "u.2.2", "x.2.2",
                        "u.1.3", "x.1.3", "u.2.3", "x.2.3")
  df1df2.[1,1] <- t2 + tau
  df1df2.[2,1] <- t2 + 2*tau
  df1df2.[1,7] <- df1df2.[1,9] <- -500
  df1df2.[2,11] <- df1df2.[2,13] <- -500
  df1df2 <- rbind(df1df2, df1df2.)
  o <- order(df1df2[,1])
  df1df2 <- df1df2[o,]
  print(df1df2)
  # points(as.numeric(df1df2[2,1]), as.numeric(df1df2[2,3]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[2,1]), as.numeric(df1df2[2,5]), pch = 1, cex = 2)
  # points(as.numeric(df1df2[3,1]), as.numeric(df1df2[3,7]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[3,1]), as.numeric(df1df2[3,9]), pch = 1, cex = 2)
  # points(as.numeric(df1df2[4,1]), as.numeric(df1df2[4,11]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[4,1]), as.numeric(df1df2[4,13]), pch = 1, cex = 2)

  # points(as.numeric(df1df2[5,1]), as.numeric(df1df2[5,3]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[6,1]), as.numeric(df1df2[6,5]), pch = 1, cex = 2)
  # points(as.numeric(df1df2[7,1]), as.numeric(df1df2[7,7]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[8,1]), as.numeric(df1df2[8,9]), pch = 1, cex = 2)
  # points(as.numeric(df1df2[9,1]), as.numeric(df1df2[9,11]), pch = 16, cex = 0.65, col = "yellow")
  # points(as.numeric(df1df2[10,1]), as.numeric(df1df2[10,13]), pch = 1, cex = 2)
  df <- data.frame(A = seq(1,6), tA = c(df1df2[2,1],df1df2[2,1],df1df2[3,1],df1df2[3,1],df1df2[4,1],df1df2[4,1]),
                   tD = c(df1df2[5,1],df1df2[6,1],df1df2[7,1],df1df2[8,1],df1df2[9,1],df1df2[10,1]))
  delay <- df[,3] - df[,2]
  df    <- cbind(df, delay)
  w <- as.numeric(df[1,3] - df[1,2])
  w <- rep(w,nveh)
  w <- df[,3] - w
  df <- cbind(df, w)
  print(df)
  plot(df[,2], df[,1], typ = "s", xlim = c(0,12), ylim = c(1,6.1),
       ylab = "Vehicle", xlab = "t, seconds")
  lines(df[,3], df[,1], typ = "s")
  abline(v = 0, col = gray(0.8))
  abline(h = 1, col = gray(0.8))
  title(main = "Railroad Track Merge", sub = "Signal Control")
  lines(c(df[1,2],df[6,2]), c(1,6), lty = 2)
  lines(c(df[1,3],df[6,3]), c(1,6), lty = 4)
  lines(c(df[1,2],df[6,2] + tau*0.5), c(1,6), lty = 5)
  print(data.frame(capacity, tau, u, t1, t2))
  legend("topleft", legend = c("A(t)","D(t)","V(t)"), lty = c(2,4,5), bty = "n")
  return(capacity)
}
