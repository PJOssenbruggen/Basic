#' The \code{bmfree} function returns a \code{tux} matrix of time \code{t}, speed \code{u} and location \code{x} values.
#'
#' @return The \code{bmfree} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The model, a stochastic traffic noise model, is assumed to be a stationary time-series model.
#' Distance \code{x} is estimated with the forward Euler method.
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
  for(i in 1:tlen) W[i] <- Z[i]
  W     <- c(0, W[-tlen])
  u     <- c(umn, umn + usd * W)[-length(tlen)]
  tux   <- as.matrix(data.frame(t = tseq, u, x, W))
  for(i in 2:tlen) {
    x[i] <- x[i-1] + dt * u[i-1]
  }
  tux[,3] <- x
  return(tux)
}
