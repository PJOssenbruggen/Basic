#' \code{findt1} returns the deceleration time \code{t1}, the location \code{xf1}, and speed \code{uf1}
#' of the following vehicle at time \code{t1}.
#'
#' @param xl0 lead vehicle location at time \code{t0}, a number.
#' @param xf0 following vehicle location at time \code{t0}, a number.
#' @param ul0 lead vehicle speed, a number.
#' @param uf0 following vehicle speed, a number.
#' @param theta gbm model parameter, a number.
#' @param leff effective vehicle length, a number.
#' @usage findt1(xl0, xf0, ul0, uf0, theta, leff)
#' @examples
#' findt1(-100, -200, 90, 90, -0.16, 14)
#' @export
findt1 <- function(xl0, xf0, ul0, uf0, theta, leff) {
  t1    <- (xl0 - xf0) / (uf0 - 1 / ul0)
  xf1   <- xf0 + uf0 * t1
  # xf11 is following vehicle location based on the speeds of the two vehicles and relative distances
  # from one another.
  uf1    <- gbm(ul0, theta, t1)
  answer <- as.matrix(data.frame(t1, xf1, uf1))
#  print(answer)

  return(answer)
}
