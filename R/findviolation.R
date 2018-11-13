#' \code{findviolation} determines if a zone violates the safe headway rule.
#'
#' @return \code{findviolation} fills the cell of the \code{dfcrit} table.
#' @param tstart start time, a number
#' @param tend end time, a number
#' @param tend.0 end time for over the long time range, a number
#' @param df1 leading vehicle, a matrix
#' @param df2 following vehicle, a matrix
#' @param step time step, a number
#' @param leff vehicle length, a number
#' @usage findviolation(tstart, tend, tend.0, df1, df2, step, leff)
# #' @examples
# #' findviolation(tstart, tend, tend.0, df1, df2, step, leff)
#' @export
findviolation <- function(tstart, tend, tend.0, df1, df2, step, leff) {
  t        <- seq(0,tend.0,step)
  df1      <- cbind(t, df1)
  df2      <- cbind(t, df2)
  tseq     <- seq(tstart,tend,step)
  df1      <- df1[df1[,1] >= tstart & df1[,1] <= tend,]
  df2      <- df2[df2[,1] >= tstart & df2[,1] <= tend,]
  tlen     <- length(tseq)
  X        <- safe <- rep(NA, tlen)
  if(is.matrix(df1) == TRUE)  {
    hdwy   <- df1[,3] - df2[,3]
    for(i in 1:tlen) safe[i] <- hsafe(df2[i,2], leff)
    for(i in 1:tlen) if(hdwy[i] >= safe[i]) X[i] <- 0 else X[i] <- 1
  } else {
    hdwy   <- df1[3] - df2[3]
    safe   <- hsafe(df2[2], leff)
    if(hdwy >= safe) X <- 0 else X <- 1
  }
  extent   <- hdwy - safe
  df       <- data.frame(X, safe, hdwy, extent, t = tseq)
  tcrit    <- min(as.numeric(df[df[,4] == min(extent),5]))
  tcrit    <- rep(tcrit, length(tseq))
  df       <- cbind(df, tcrit)
  return(df)
}
