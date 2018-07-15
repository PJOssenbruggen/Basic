#' \code{vehtfix} selects a information over a time range about vehicle \code{veh}
#' of the \code{df} matrix for analysis with the \code{brktrials2} function.
#'
#' @param veh vehicle, a number
#' @param nveh number of vehicle in \code{df}, a number
#' @param df a data frame created in\code{bk2trials}, a data frame
#' @param tstart time, a number
#' @param tend time, a number
#' @param step size in seconds, a number
#' @param ufix new speeds, a vector
#' @param xfix new locations, a vector
#' @usage vehtfix(veh, nveh, df, tstart, tend, step, ufix, xfix)
#' @export
# #' @examples
# #' vehtfix(veh, df)
vehtfix <- function(veh, nveh, df, tstart, tend, step, ufix, xfix) {
  df.     <- vehdf(veh, nveh, df)
  tstart. <- tstart
  tend.   <- tend
  tseq    <- seq(tstart., tend., by = step)
  df.[df.[,1] >= tstart. & df.[,1] <= tend.,3] = xfix
  df.[df.[,1] >= tstart. & df.[,1] <= tend.,2] = ufix
  ufix   <- df.[,2]
  xfix   <- df.[,3]
  df     <- vehfix(veh, nveh, ufix, xfix, df)
  return(df)
}
