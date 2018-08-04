#' \code{findt2} returns the time \code{t2} and \code{xf2} when the following vehicle driver recognizes
#' the lead vehicle has decelerated.
#'
#' @param L lead vehicle information, a matrix
#' @param theta a \code{gbm} model parameter, a number
#' @param t4 time, a number
#' @usage findt2(L, theta, t4)
findt2 <- function(L, theta, t4) {
  t2  <- NULL
  t0  <- L[1,1]
  t3  <- L[1,4]
  xl0 <- L[3,1]
  ul0 <- L[2,1]
  ul3 <- L[2,4]
  xl3 <- L[3,4]
  tseq  <- seq(t0, t3, by = 0.01)
  tlong <- length(tseq)
  xseq  <- rep(NA, tlong)
  u0 = ul0
  for(i in 1:tlong) xseq[i] <- xl0 + gbmx(u0, theta, t = tseq[i])
#  lines(tseq, xseq, col = "blue")
  xl0 <- L[3,1]
  xf1 <- L[3,1] - L[4,1]
  t1  <- t0
  f11 <- f1(xl0, ul0, theta, xf1, t = t1, t1)
  f13 <- f1(xl0, ul0, theta, xf1, t = t3, t1)
  f21 <- f2(xl0, ul0, ul3, theta, t3, xf1, t = t3, t1)
  f24 <- f2(xl0, ul0, ul3, theta, t3, xf1, t = t4, t1)
  s1 <- sign(f11)
  s2 <- sign(f13)
  if(s1 != s2) {
    t2 <- uniroot(f1, xl0 = xl0, ul0 = ul0, theta = theta, xf1 = xf1, t1 = t1,
                 interval = c(t1, t3), tol = 1e-9)$root
  }	else {
    t2 <- uniroot(f2, xl0 = xl0, ul0 = ul0, ul3 = ul3, theta = theta, t3 = t3, xf1 = xf1, t1 = t1,
                 interval = c(t3, t4), tol = 1e-9)$root
  }
  # ul2 and xl2 are the speed and location of the lead vehicle at time 2
  if(t2 <= t3) ul2 <- gbm(ul0, theta, t2) else ul2 = ul3
  u0 <- ul0
  if(t2 <= t3) xl2 <- xl0 + gbmx(u0, theta, t2) else xl2 <- xfollow(xl3, ul3, t2, t3)
  answer <- c(t2, ul2, xl2)
#  print(answer)
#  points(t2,xl2, cex = 0.5, pch = 8)
  return(answer)
}
