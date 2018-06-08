#' The \code{bmfree} function returns a \code{tux} matrix of time \code{t}, speed \code{u} and location \code{x} values.
#'
#' @return The \code{bmfree} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The model, a stochastic traffic noise model, is assumed to be a Brownian bridge model.
#' Distance \code{x} is estimated with the forward Euler method.
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param T upper time range, a number
#' @param dt time-step, a number
#' @usage bmfree(umn, usd, T, dt)
#' @examples
#' bmfree(41, 11, 100, 1)
#' bmfree(2, 2, 60, 1)
bmfree  <- function(umn, usd, T, dt) {
  umn   <- umn*5280/3600
  usd   <- usd*5280/3600
  tseq  <- seq(0, T, by = dt)
  N     <- length(tseq)
  x     <- c(0, rep(NA, tlen - 1))
  u     <- as.numeric(BBridge(x = umn, y = umn, t0 = 0, N, T))
  for(i in 1:tlen) if(u[i] <= 0) u[i] = 0
  for(i in 2:tlen) x[i] <- x[i-1] + dt * u[i-1]
  tux   <- as.matrix(data.frame(t = tseq, u, x))
  return(tux)
}
