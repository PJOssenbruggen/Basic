#' \code{nopass} produces \code{t}, \code{u} and \code{x} for lead following vehicles downstream of a bottleneck
#'
#' @return \code{brktrials} returns  a data frame for a vehicle at a bottleneck.
#' \code{brktrials} is a wrapper function of \code{accelpass}.
#' @param veh vehicle, a number
#' @param nope is a data.frame of leading and following time, speed and location data, a dataframe
#' @param merge.df is a data.frame of leading and following time, speed and location data, a dataframe
#' @param step size in seconds, a number
#' @usage nopass(nope, merge.df, step)
nopass <- function(veh, nope, merge.df, step) {
  # colnames(nope) <- c("t", "u.lead","x.lead","u.follow","x.follow")

  lines(nope[,1], nope[,5], col = gray(0.5))
  hsafedown   <- violate <- u.fix <- x.fix <- rep(NA, dim(nope)[1])
  for(j in 1:dim(nope)[1]) {
    u <- as.numeric(nope[j,4])
    hsafedown[j] <- hsafe(u, leff)
  }
  hobsdown <- nope[,3] - nope[,5]
  for(j in 1:dim(nope)[1]) {
    if(hobsdown[j] < hsafedown[j]) {
      violate[j]  <- 1
      u.fix[j]    <- nope[j,2]
      if(j == 1) x.fix[j] <- merge.df[dim(merge.df)[1],3]
      else {
        u         <- as.numeric(nope[j,2])
        hsafe     <- hsafe(u, leff)
        x.fix[j]  <- nope[j,3] - hsafe
      }
    }  else {
      violate[j] <- 0
      u.fix[j]   <- nope[j,4]
      x.fix[j]   <- nope[j,5]
    }
  }
  lines(nope[,1], x.fix, col = gray(0.5), lty = 3, lwd = 2)
  lines(nope[,1], nope[,3], col = gray(0.), lty = 3, lwd = 4)
  nope  <- cbind(nope, hobsdown, hsafedown, violate, u.fix, x.fix)
 # if(veh == 4) browser()
  return(nope)
}
