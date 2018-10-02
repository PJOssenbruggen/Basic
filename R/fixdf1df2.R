#' \code{fixdf1df2} returns a matrix with following vehicle information.
#'
#' @return A matrix of speed, location data by time.
#' @param veh, a number
#' @param df2.fix output from \code{fixviolation}, matrix
#' @param df1df2, a matrix
#' @usage fixdf1df2(veh, df2.fix, df1df2)
#' @export
fixdf1df2 <- function(veh, df2.fix, df1df2) {
  u              <- df2.fix[,1]
  x              <- df2.fix[,2]
  y              <- df2.fix[,3]
  ucol           <- 3*(veh-1) + 1
  xcol           <- 3*(veh-1) + 2
  ycol           <- 3*(veh-1) + 3
  df1df2[,ucol]  <- u
  df1df2[,xcol]  <- x
  df1df2[,ycol]  <- y
  return(df1df2)
}
