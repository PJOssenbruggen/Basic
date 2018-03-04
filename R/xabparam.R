#' Estimate the parameters \code{a} and \code{b} of a second-order velocity model.
#'
#' @param tstart time, a number
#' @param tend time, a number
#' @param  ustart speed, a number
#' @param  uend speed, a number
#' @param  xstart location, a number
#' @param  xend location, a number
#' @usage  xabparam(tstart, tend, ustart, uend, xstart, xend)
#' @examples
#' xabparam(0, 18.59797, 92.4, 0, 0, 1397.045)
xabparam = function(tstart, tend, ustart, uend, xstart, xend) {
  dt = tend - tstart
  du = uend - ustart
  dx = xend - xstart - ustart * dt
  A = matrix(c(dt, 0.5*dt^2, -0.5*dt^2, -dt^3/6), ncol = 2)
  B = matrix(c(du, dx), ncol = 1)
  a = solve(A,B)[1]
  b = solve(A,B)[2]
  return(c(a,b))
}
