#' The \code{bmfree} function returns a \code{tux} matrix of time \code{t}, speed \code{u} and location \code{x} values.
#'
#' @return The \code{bmfree} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The function uses a stochastic traffic noise model, a brownian motion or Weiner \code{W} model.
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param T upper time range, a number
#' @param dt time-step, a number
#' @usage bmfree(umn, usd, T, dt)
#' @examples
#' bmfree(34, 3, 60, 5)
#' bmfree(2, 2, 60, 5)
bmfree  <- function(umn, usd, T, dt) {
  umn   <- umn*5280/3600
  usd   <- usd*5280/3600
  tseq  <- seq(0, T, by = dt)
  tlen  <- length(tseq)
  W     <- numeric(tlen)
  Z     <- rnorm(T)
  x     <- c(0, rep(NA, tlen - 1))
  tau   <- tseq/T
  W     <- numeric(tlen)
  for(i in 1:tlen) W[i] <- Z[i] * sqrt(dt)
  W     <- c(0, W[-tlen])
  noise <- data.frame(W)
  bm    <- c(umn, umn + usd * W)[-length(tlen)]
  for(i in 1:length(bm)) if(bm[i] < 0) bm[i] <- 0
  theta <- rep(NA, tlen)
  tux   <- as.matrix(data.frame(t = tseq, u = bm, x, theta))
# Find theta for each dt time interval
  for(i in 2:tlen) {
    upre       <- tux[i - 1, 2]
    u0         <- tux[i, 2]
    t3         <- dt
    xstart     <- tux[i-1, 3]
    if(upre == 0 & u0 == 0) for(j in 1:i) {
      if(tux[i - 1 - j, 2] > 0 & i > j) {
        upre <- tux[i - 1 - j, 2]
        t3   <- j * dt
        break
      } else {
        upre <- umn
        break
      }
    }
    theta      <- gbmtheta(upre, u0, t3)
    tux[i,4]   <- theta
    tux[i,3]   <- xstart + gbmx(upre, theta, dt)
  }
  return(tux)
}
