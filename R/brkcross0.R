#' \code{brkcross0} estimates of speed and location of vehicle crossing bottleneck location \code{x}
#'
#' @param i index, a number
#' @param dftest, a dataframe
#' @usage brkcross0(i, dftest)
#' @export
brkcross0 <- function(i, dftest) {
  xabs <- abs(dftest[,3])
  xseq <- dftest[,3]
  useq <- dftest[,2]
  tseq <- dftest[,1]
  x0tst   <- min(xabs)
  t0   <- tseq[xabs == x0tst]
  x0   <- xseq[xabs == x0tst]
  u0   <- useq[xabs == x0tst]
  df   <- data.frame(i, t0, u0, x0)
  return(df)
}
