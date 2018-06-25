#' Estimate speed \code{u} at time \code{t} using a geometric Brownian motion model.
#'
#' @param u0 initial speed (fps), a number
#' @param theta, a number
#' @param t time (seconds), a number
#' @usage gbm(u0, theta, t)
# #' @examples
# #' gbm(92.4, -0.0154, 40)
# #' gbm(0, 0.4, 4)
gbm = function(u0, theta, t) {
  if(u0 > 0) {
    u = u0 * exp(theta * t)
    return(u)
  }
  if(u0 == 0) {
    u = exp(theta * t)
    return(u)
  }
}

