#' The \code{findt1ab} returns the time \code{tf1} and the location \code{xf1} and speed \code{uf1}
#' of the following vehicle when the driver begins to decelerate.
#'
#' @param xl0 lead vehicle location at time \code{t0}, a number
#' @param xf0 following vehicle location at time \code{t0}, a number
#' @param ul0 lead vehicle speed at time \code{t0}, a number
#' @param uf0 following vehicle speed at time \code{t0}, a number
#' @param tseq time sequence of the lead vehicle \code{l}, a vector
#' @param useq speed sequence of the lead vehicle \code{l}, a vector
#' @param t1 start time, a number
#' @param t2 end time, a number
#' @usage findt1ab(xl0, xf0, uf0, ul0, tseq, useq, t1, t2)
#' @export
findt1ab <- function(xl0, xf0, uf0, ul0, tseq, useq, t1, t2) {
  tf1      <- (xl0 - xf0) / (uf0 - 1 / ul0) + t1
  xl1      <- xf0 + uf0 * (tf1 - t1)
  tseq     <- unlist(tseq)
  useq     <- unlist(useq)
  uf1      <- min(useq[tseq < tf1])
# xf1 is following vehicle location based on the speeds of the two vehicles and relative distances
# from one another and maintaining a safe headway. Assumption: uf1 = ul1.
  answer   <- as.matrix(data.frame(tf1 = tf1, xl1 = xl1, uf1 = uf1))
  return(answer)
}
