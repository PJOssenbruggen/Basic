#' \code{nopass} produces \code{t}, \code{u} and \code{x} for lead and following vehicles downstream of a bottleneck
#'
#' @return \code{brktrials} returns a data frame speed and location downstream of bottleneck.
#' @param veh vehicle, a number
#' @param nope is a data.frame of leading and following vehicles: time, speed and location data, a data frame
#' @param leff effective vehicle length in feet, a number
#' @usage nopass(veh, nope, leff)
#' @export
nopass <- function(veh, nope, leff) {
  hcrit   <- u <- x <- rep(NA, dim(nope)[1])
  for(j in 1:dim(nope)[1]) {
    u <- as.numeric(nope[j,4])
    hcrit[j] <- hsafe(u, leff)
  }
  hobs <- nope[,3] - nope[,5]
  for(j in 1:dim(nope)[1]) {
    if(hobs[j] < hcrit[j]) {
      u.      <- as.numeric(nope[j,2])
      u[j]    <- nope[j,2]
      hfix    <- hsafe(u., leff)
      x[j]    <- nope[j,3] - hfix
    }  else {
      u[j]   <- nope[j,4]
      x[j]   <- nope[j,5]
    }
  }
  fix  <- data.frame(u, x)
  return(fix)
}
