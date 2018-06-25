#' Headway \code{h} between the lead and following vehicles at time \code{t}. Lead and following vehicles use constant speed models, respectively.
#'
#' @param xl0 location of lead vehicle at time \code{t} = \code{t0}, a number
#' @param ul0 pre-breakdown speed of lead vehicle, a number
#' @param ul3 breakdown speed of lead vehicle, a number
#' @param theta gbm model parameter, a number
#' @param t3 transition time, a number
#' @param xf1 location of following vehicle at time \code{t} = \code{t1}, a number
#' @param t time, a number
#' @param t1 deceleration time, a number
#' @usage f2(xl0, ul0, ul3, theta, t3, xf1, t, t1)
# #' @examples
# #' f2(-420, 92.4, 49.86667, -0.0154, 40, -600, 45, 0)
f2 <- function(xl0, ul0, ul3, theta, t3, xf1, t, t1) {
  h = xl0 + gbmx(ul0, theta, t3) + ul3 * (t - t3) - xfollow(xf1, ul0, t, t1)
  return(h)
}

