#' Space headway \code{h} between the lead and following vehicles at time \code{t}. Lead and following vehicles use gBm and constant speed models, respectively.
#'
#' @param xl0 location of lead vehicle at \code{t} = 0, a number
#' @param ul0 pre-breakdown speed of lead vehicle, a number
#' @param theta a \code{gbm} model parameter, a number
#' @param xf1 location of following vehicle at \code{t} = \code{t1}, a number
#' @param t1 deceleration time, a number
#' @param t time, a number
#' @usage f1(xl0, ul0, theta, xf1, t, t1)
# #' @examples
# #' f1(-689.6, 92.4, -0.0154, -240, 4, 0)
f1 <- function(xl0, ul0, theta, xf1, t, t1) {
  x0 <- xf1
  u  <- ul0
  t0 <- t1
  h <- xl0 + gbmx(ul0, theta, t) - xfollow(x0, u, t, t0)
  return(h)
}
