#' Estimate \code{theta} of a geometric Brownian motion model.
#'
#' @param upre pre-breakdown speed, a number
#' @param u0 breakdown speed, a number
#' @param t3 time when u0 is initiated, a number
#' @usage gbmtheta(upre, u0, t3)
# #' @examples
# #' gbmtheta(63, 34, 4)
# #' gbmtheta(0, 5, 4)
# #' gbmtheta(6, 0, 4)
gbmtheta = function(upre, u0, t3) {
  if(u0 == 0) {
    theta <- log(0.01*upre/upre)/t3
    return(theta)
  }
  if(u0 != 0 & upre != 0) {
    theta <- log(u0/upre)/t3
    return(theta)
  }
  if(upre == 0) {
    theta <- log(u0)/t3
    return(theta)
  }
}
