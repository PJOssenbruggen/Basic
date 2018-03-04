#' \code{rrheadway} is a function to estimate the traffic flow \code{q} on a ring road.
#'
#' @return The \code{rrheadway} uses the output matrix \code{LF} from \code{rrtrials}.
#' @param LF location \code{x} output from \code{rrtrials}, a matrix.
#' @param dt time-step, a number.
#' @usage rrheadway(LF, dt)
rrheadway <- function(LF, dt) {
  I   <- dim(LF)[1]
  K   <- dim(LF)[2] - 1
  t5s <- rep(NA, K)
  for(k in 1:K) {
    for(i in 1:I) {
      if(k == K & i == I) return(t5s)
      s1 <- sign(LF[i, k + 1])
      s2 <- sign(LF[i + 1, k + 1])
      if(s1 != s2) {
        t1  <- LF[i,1]
        dx  <- LF[i + 1,k + 1] - LF[i,k + 1]
        dx0 <- abs(LF[i,k + 1])
        t5  <- t1 + dt * dx0 / dx
        t5s[k] <- t5
        break
      }
      if(i + 1 == I) break
    }
  }
  return(t5s)
}
