#' Estimate \code{x} distance traveled using a constant speed model.
#'
#' @param x0 starting location at time \code{t0}, a number
#' @param u speed, a number
#' @param t travel time, a number
#' @param t0 deceleration time, a number
#' @usage  xfollow(x0, u, t, t0)
# #' @examples
# #' xfollow(-200, 80, 10, 0)
xfollow <- function(x0, u, t, t0) {
  x <- x0 + u * (t - t0)
  return(x)
}

