#' \code{brkcross0} estimates the time, speed and location that a vehicle crosses bottleneck location \code{x} = 0.
#'
#' @param vehicle, a number
#' @param df, a dataframe
#' @usage brkcross0(vehicle, df)
#' @export
brkcross0 <- function(vehicle, df) {
  xabs <- abs(df[,3])
  xseq <- df[,3]
  useq <- df[,2]
  tseq <- df[,1]
  x0tst   <- min(xabs)
  t0   <- tseq[xabs == x0tst]
  x0   <- xseq[xabs == x0tst]
  u0   <- useq[xabs == x0tst]
  df   <- data.frame(vehicle, t0, u0, x0)
  return(df)
}
