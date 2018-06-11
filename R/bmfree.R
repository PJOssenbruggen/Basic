#' The \code{bmfree} function returns a \code{tux} matrix of time \code{t}, speed \code{u} and location \code{x} values.
#'
#' @return The \code{bmfree} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The model, a stochastic traffic noise model, is assumed to be a Brownian bridge model.
#' Distance \code{x} is estimated with the forward Euler method.
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param T upper time range in minutes, a number
#' @param N number of time-steps, a number
#' @param lambda calibration constant, a number
#' @usage bmfree(umn, usd, N, T, lambda)
#' @examples
#' bmfree(41, 11, 900, 60, 1.15)
#' bmfree(18.8, 3.8, 60, 60, 0.5)
bmfree  <- function(umn, usd, N, T, lambda) {
  umn   <- umn*5280/3600
  usd   <- usd*5280/3600
  W     <- numeric(N+1)
  t     <- seq(0, T, length = N+1)
  for(i in 2:(N+1)) W[i] <- W[i-1] + rnorm(1)
  x     <- umn
  y     <- umn
  u     <- x + (W - t/T *  (W[N+1] - y + x)) * lambda
  dt    <- T/N
  x     <- rep(0, length(t))
  for(i in 1:(N+1)) if(u[i] <= 0) u[i] = 0
  for(i in 2:(N+1)) x[i] <- x[i-1] + dt * u[i-1]
  tux   <- as.matrix(data.frame(t, u, x))
  return(tux)
}
