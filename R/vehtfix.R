#' \code{vehtfix} fixes \code{hsafe} violations for vehicle \code{veh}.
#'
#' @param veh vehicle, a number
#' @param nveh number of vehicle in \code{df}, a number
#' @param df a data frame created in\code{bk2trials}, a data frame
#' @param tstart time, a number
#' @param tend time, a number
#' @param delt size in seconds, a number
#' @param ufix new speeds, a vector
#' @param xfix new locations, a vector
#' @usage vehtfix(veh, nveh, df, tstart, tend, delt, ufix, xfix)
#' @export
# #' @examples
# #' vehtfix(veh, df)
vehtfix <- function(veh, nveh, df, tstart, tend, delt, ufix, xfix) {
  df.     <- vehdf(veh, nveh, df)
  tstart. <- tstart
  tend.   <- tend
  tseq    <- seq(tstart., tend., by = delt)
  df.[df.[,1] >= tstart. & df.[,1] <= tend.,3] = xfix
  df.[df.[,1] >= tstart. & df.[,1] <= tend.,2] = ufix
  ufix   <- df.[,2]
  xfix   <- df.[,3]
  df     <- vehfix(veh, nveh, ufix, xfix, df)
  return(df)
}
