#' The \code{bmfree} function returns a \code{tux} matrix of time \code{t}, speed \code{u} and location \code{x} values.
#'
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param Tup upper time range in minutes, a number
#' @param N number of time-steps, a number
#' @usage bmfree(umn, usd, N, Tup)
# #' @examples
# #' bmfree(41, 11, 900, 60)
# #' bmfree(18.8, 3.8, 60, 60)
bmfree  <- function(umn, usd, N, Tup) {
  umn   <- umn*5280/3600
  usd   <- usd*5280/3600
  W     <- numeric(N+1)
  t     <- seq(0, Tup, length = N+1)
  for(i in 2:(N+1)) W[i] <- W[i-1] + rnorm(1)
  x     <- umn
  y     <- umn
  u     <- x + (W - t/Tup *  (W[N+1] - y + x))
  dt    <- Tup/N
  x     <- rep(0, length(t))
  for(i in 1:(N+1)) if(u[i] <= 0) u[i] = 0
  for(i in 2:(N+1)) x[i] <- x[i-1] + dt * u[i-1]
  tux   <- as.matrix(data.frame(t, u, x))
  return(tux)
}
