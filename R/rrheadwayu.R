#' \code{rrheadwayu} is a function used to find the speed \code{u} on a ring road at \code{x = 0}.
#'
#' @return The \code{rrheadwayu} uses the output matrix \code{LFu} from \code{rrtrials} and code{t5}.
#' @param LFu speed \code{u} output from \code{rrtrials}, a matrix.
#' @param t5 time when vehicle crosses the line \code{x = 0}, a vector.
#' @usage rrheadwayu(LFu, t5)
rrheadwayu <- function(LFu, t5) {
  us   <- {}
  u    <- NA
  T    <- LFu[dim(LFu)[1],1]
  dt   <- LFu[2,1]
  nveh <- dim(LFu)[2] - 1
  tseq <- seq(0, T, dt)
  tlen <- length(tseq)
  for(k in 1:nveh) {
    for(i in 2:tlen) {
      t   <- LFu[i, 1]
      tup <- t5[k]
      if(t >= tup) {
        upre  <- LFu[i - 1, k + 1]
        u0    <- LFu[i, k + 1]
        theta <- gbmtheta(upre, u0, dt)
        t     <- t5[k] - LFu[i - 1,1]
        u     <- gbm(upre, theta, t)
        us    <- c(us, u)
        break
      }
    }
  }
  return(us)
}
