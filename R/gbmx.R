#' Estimate distance \code{x} given a geometric Brownian motion model and time t.
#'
#' @param u0 initial speed, a number
#' @param theta gbm model estimate, a number.
#' @param t time, a number
#' @usage  gbmx(u0, theta, t)
# #' @examples
# #' gbmx(92.4, -0.0154, 10)
# #' gbmx(0, 0.4, 10)
# #' gbmx(6, -0.154, 4)
gbmx = function(u0, theta, t) {
  if(u0 > 0) {
    x <- u0/theta*(exp(theta*t) - 1)
    return(x)
    }
  if(u0 == 0) {
    x <- exp(1)^(theta * t) - 1
    return(x)
  }
}

