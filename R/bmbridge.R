#' The \code{bmbridge} function returns a \code{tux} matrix of time \code{t}, speed \code{u} (fps)
#'  and location \code{x} (feet) for upstream traffic.
#'
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{usd} (mph), a number
#' @param tstart simulation start time, a number
#' @param tend simulation end time, a number
#' @param xstart location (feet), a number
#' @param type logical for plotting where TRUE creates a plot
#' @param delt time-step size, a number
#' @usage bmbridge(umn, usd, tstart, tend, xstart, delt, type)
# #' @examples
# #' bmbridge(41, 11, 16.5, 30, 10, 0.25, TRUE)
bmbridge  <- function(umn, usd, tstart, tend, xstart, delt, type) {

  t     <- seq(tstart, tend, delt)
  N     <- length(t)
  W     <- numeric(N)
  usd   <- 5280/3600*usd
  for(i in 2:(N)) W[i] <- W[i-1] + usd * sqrt(delt) * rnorm(1)
  x     <- 5280/3600*umn
  y     <- 5280/3600*umn
#  u     <- BBride
  for(i in 1:N) if(u[i] <= 0) u[i] = 0
  if(type == TRUE) {
    plot(t, u, typ = "l")
    abline(h = x, col = gray(0.8))
  }
  x     <- rep(0, length(t))
  x[1]  <- xstart
  for(i in 2:N) x[i] <- x[i-1] + delt * u[i-1]
  tux   <- as.matrix(data.frame(t, u, x))
  return(tux)
}
