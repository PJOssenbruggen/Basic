#' \code{zoneviolation} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
#'
#' @return The analyses the zone for a safe headway violation. \code{X} = 0 if safe, else non-zero.
#' @param df1df2 a data set, a matrix
#' @param zone, a number
#' @param veh identifier, a number
#' @param dfcrit a table of vehicle information, a matrix
#' @param delt time-step, a number
#' @param tend.0 end time for over the long time range, a number
#' @param leff vehicle length in feet, a number
#' @usage zoneviolation(veh, zone, df1df2, dfcrit, delt, tend.0, leff)
# #' @examples
# #' zoneviolation(veh, zone, df1df2, dfcrit, delt, tend.0, leff)
#' @export
zoneviolation <- function(veh, zone, df1df2, dfcrit, delt, tend.0, leff) {
  if(veh == 2) dfcrit[1,c(6:8)] <- c(NA, NA, NA)
  ucol   <- 3*(veh-1) - 2
  xcol   <- 3*(veh-1) - 1
  ycol   <- 3*(veh-1)
  ucol.  <- 3*veh - 2
  xcol.  <- 3*veh - 1
  ycol.  <- 3*veh
  df1    <- cbind(df1df2[,ucol], df1df2[,xcol], df1df2[,ycol])
  df2    <- cbind(df1df2[,ucol.], df1df2[,xcol.], df1df2[,ycol.])
  ncol   <- 1 + zone
  tstart <- as.numeric(dfcrit[veh,ncol])
  ncol   <- ncol + 1
  tend   <- as.numeric(dfcrit[veh,ncol])
  df     <- findviolation(tstart, tend, tend.0, df1, df2, delt, leff)
  X      <- sum(df[,1])
  ncol   <- 5 + zone
  dfcrit[veh,ncol] <- X
  return(dfcrit)
}
