#' \code{plottux} produces \code{t-u} and \code{t-x} trajectories for a lead and following vehicles on a ring road,
#'
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param T upper time range, a number
#' @param dt time-step, a number
#' @param k0 traffic density (vpm), a number
#' @usage plottux(umn, usd, T, dt, k0)
#' @examples
#' plottux(2,2,100,1,55)
#' plottux(41,11,100,1,55)
plottux <- function(umn, usd, T, dt, k0) {
  umn0  <- t(data.frame(umn = 5280/3600*umn, 0))
  t0    <- 0
  lead  <- bmfree(umn, usd, T, dt)
  foll  <- bmfree(umn, usd, T, dt)
  u <- c(lead[,2], foll[,2])
  par(mfrow = c(1,2))
  xlim <- c(0,T)
  ylim <- c(0,max(u))
  nobs  <- dim(lead)[1]
  h0    <- 5280/k0
  foll[,3]  <- foll[,3] - rep(h0, nobs)
  h     <- lead[,3] - foll[,3]
  tseq  <- lead[,1]
  hsf   <- rep(NA, nobs)
  for(i in 1:nobs) hsf[i] <- hsafe(foll[i,2], leff)
  df <- data.frame(t = tseq, xl = lead[,3], xf = foll[,3], hsf, h, ul = lead[,2], uf = foll[,2],
                   uf0 = foll[,2], xf0 = foll[,3])
  xl    <- lead[,3]
  ul    <- lead[,2]
  plot(df[,1], df[,6], xlab = "t, seconds", ylab = expression(u[t]*", fps"),
       typ = "l",xlim, ylim*1.5, lwd = 2)
  lines(df[,1], df[,7], lwd = 1)

# headway analysis: If safe, no adjustments are made to spacing or speed of the following vehicle are made.
  for(i in 1:nobs) if(df[i,5] > df[i,4]) df[i,3] <- df[i,3] else {
     df[i,3] <- df[i,2] - df[i,4]
     df[i,7] <- (df[i,3] - df[i-1,3])/dt
  }
  ul <- df[,6]
  uf <- df[,7]
  xl <- df[,2]
  xf <- df[,3]
  u <- c(ul, uf)
  x <- c(xl, xf)
  ylimx = c(min(x),max(x))
  plot(df[,1], df[,6], xlab = "t, seconds", ylab = expression(u[t]*", fps"),
       typ = "l",xlim, ylim*1.5, lwd = 2)
  lines(df[,1], df[,7], lwd = 1)
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))

# t-x diagram
  plot(df[,1], df[,2], typ = "l", xlab = "t, seconds", ylab = expression(x[t]*", feet"),
       xlim = xlim, ylim = ylimx,lwd = 2)
  lines(c(0,T), c(0, 5280/3600*umn * T), col = gray(0.5), lty = 3)
  lines(df[,1], df[,3],lwd = 1)
  abline(h = 0, col = gray(0.8))
  abline(v = 0, col = gray(0.8))
  legend("topleft",
         legend = c("Leader", "Follower"),
         lty = c(1,1),
         lwd = c(2,1),
         bty = 'n')
  dfsum = rbind(apply(df[,c(6,7,8)],2,mean), apply(df[,c(6,7,8)],2,sd))

  dfsum = as.data.frame(cbind(umn0, dfsum))
  colnames(dfsum) <- c("umn", "ul", "uf", "uf0")
  rownames(dfsum) <- c("mean", "sd")
  dfsum
  return(dfsum)
}
