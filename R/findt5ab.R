#' The time \code{t5} when the following vehicle reaches the bottleneck or location \code{x} = 0.
#'
#' @param xf1 location of the following vehicle at time \code{t1}, a number
#' @param xf2 location of the following vehicle at time \code{t2}, a number
#' @param uf1 speed of the following vehicle at time \code{t1}, a number
#' @param uf2 speed of the following vehicle at time \code{t2}, a number
#' @param t1 deceleration time, a number
#' @param t2 time when the vehicle sights lead vehicle, a number
#' @param t4 upper end of search range, a number
#' @usage findt5ab(xf1, xf2, uf1, uf2, t1, t2, t4)
#' @export
findt5ab <- function(xf1, xf2, uf1, uf2, t1, t2, t4) {
  tstart <- t1
  tend   <- t2
  ustart <- uf1
  uend   <- uf2
  xstart <- xf1
  xend   <- xf2
  a   <- xabparam(tstart, tend, ustart, uend, xstart, xend)[1]
  b   <- xabparam(tstart, tend, ustart, uend, xstart, xend)[2]
  f11 <- xab(x0 = xf1, u0 = uf1, a = a, b = b, t = t1, t0 = t1)
  f13 <- xab(x0 = xf1, u0 = uf1, a = a, b = b, t = t2, t0 = t1)
  s1  <- sign(f11)
  s2  <- sign(f13)
  if(s1 != s2) {
    t5 <- uniroot(xab, x0 = xf1, u0 = uf1, a = a, b = b, t0 = t1,
                 interval = c(t1, t2), tol = 1e-9)$root
  }	else {
    if(uf2 == 0) t5 <- NA else {
      t4 <- 1000
      t5 <- uniroot(xfollow, x0 = xf2, u = uf2, t0 = t2,
                    interval = c(t2, t4), tol = 1e-9)$root
    }
  }
  if(!is.na(t5)) points(t5,0, pch = 16, cex = 0.5)
  return(t5)
}
