#' Estimate distance \code{x} using second-order speed function.
#'
#' @param a second-order speed function parameter, a number
#' @param b second-order speed function parameter, a number
#' @param t0 initial time \code{t0}, a number
#' @param t forecast time, a number
#' @param x0 location at \code{t0}, a number
#' @param u0 speed at \code{t0}, a number
#' @usage xab(x0,u0,a,b,t,t0)
# #' @examples
# #' xab(0,0,16,0,10,0)
xab = function(x0,u0,a,b,t,t0) x0 + u0 * (t-t0) + a/2 * (t-t0)^2 - b/6 * (t-t0)^3
