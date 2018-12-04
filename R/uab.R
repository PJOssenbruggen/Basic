#' Estimate distance \code{u} using second-order speed function.
#'
#' @param a second-order speed function parameter, a number
#' @param b second-order speed function parameter, a number
#' @param t0 initial time \code{t0}, a number
#' @param t forecast time, a number
#' @param u0 speed at \code{t0}, a number
#' @usage uab(u0,a,b,t,t0)
#' @export
uab = function(u0,a,b,t,t0) u0 + a * (t-t0) - b/2 * (t-t0)^2
