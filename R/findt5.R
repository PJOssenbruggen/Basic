#' The time \code{t5} when the following vehicle reaches the bottleneck or location \code{x} = 0.
#'
#' @param xf1 location of the following vehicle at \code{t1}, a number
#' @param xf3 location of the following vehicle at \code{t3}, a number
#' @param uf1 speed of the following vehicle at \code{t1}, a number
#' @param uf3 speed of the following vehicle at \code{t3}, a number
#' @param theta gbm model parameter of the following vehicle, a number
#' @param t1 deceleration time, a number
#' @param t2 time when the vehicle sights lead vehicle, a number
#' @param t3 time when the vehicle obtains breakdown speed \code{u0}, a number
#' @param t4 upper end of search range, a number
#' @param t unknown time \code{t5}, a number
#' @usage findt5(xf1, xf3, uf1, uf3, theta, t, t1, t2, t3, t4)
#' @export
findt5 <- function(xf1, xf3, uf1, uf3, theta, t, t1, t2, t3, t4) {
  u0   <- uf1
  fun3 <- function(xf1, uf1, theta, t, t1) xf1 + gbmx(uf1, theta, t - t1)
  f11  <- fun3(xf1, uf1, theta, t1, t1)
  f13  <- fun3(xf1, uf1, theta, t3, t1)
  s1  <- sign(f11)
  s2  <- sign(f13)
  if(s1 != s2) {
    t5  <- uniroot(fun3, xf1 = xf1, uf1 = uf1, theta = theta, t1 = t1,
        interval = c(t1, t3), tol = 1e-9)$root
  }	else {
    t5  <- uniroot(xfollow, x0 = xf3, u = uf3, t1 = t3,
            interval = c(t3, t4), tol = 1e-9)$root
  }
  return(t5)
}
