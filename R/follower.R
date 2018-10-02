#' \code{follower} returns a matrix of the following vehicle
#'
#' @return A matrix of speed, location data by time.
#' @param veh, a number
#' @param df1df2, a matrix
#' @usage follower(veh, df1df2)
#' @export
follower <- function(veh, df1df2) {
  ucol   <- 3*(veh-2) + 1
  xcol   <- 3*(veh-2) + 2
  ycol   <- 3*(veh-2) + 3
  u      <- df1df2[,ucol]
  x      <- df1df2[,xcol]
  y      <- df1df2[,ycol]
  df1    <- data.frame(u,x,y)
  ucol   <- 3*(veh-1) + 1
  xcol   <- 3*(veh-1) + 2
  ycol   <- 3*(veh-1) + 3
  u      <- df1df2[,ucol]
  x      <- df1df2[,xcol]
  y      <- df1df2[,ycol]
  df2    <- as.matrix(data.frame(u,x,y))
  return(df2)
}
