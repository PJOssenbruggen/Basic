#' \code{violations} produces of safe and unsafe headways for a pair of lead and following vehicles.
#'
#' @return \code{violations} is used to determine a following vehicle is violating the safe headway rule.
#' \code{violations} returns a logical, TRUE when a violation occurs and FALSE otherwise.
#' @param df1 leading vehicle, a matrix
#' @param df2 following vehicle, a matrix
#' @param leff vehicle length, a number
#' @usage violations(df1, df2, leff)
# #' @examples
# #' violations(df1,df2,leff)
#' @export
violations <- function(df1,df2,leff) {
  hlen     <- dim(df1)[1]
  viol     <- safe <- rep(NA, hlen)
  hdwy     <- round(df1[,3] - df2[,3],0)
  for(i in 1:hlen) safe[i] <- round(hsafe(df2[i,2], leff),0)
  for(i in 1:hlen) if(hdwy[i] >= safe[i]) viol[i] <- 0 else viol[i] <- 1
  extent   <- safe - hdwy
  df       <- data.frame(viol, safe, hdwy, extent)
  return(df)
}
