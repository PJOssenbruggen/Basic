#' \code{vehfix} fixes \code{hsafe} violation with speed \code{ufix} and location \code{xfix} and \code{yfix} vectors from \code{brktrials2}
#'
#' @param veh vehicle, a number
#' @param nveh number of vehicle in \code{df}, a number
#' @param ufix speed, a vector
#' @param xfix location, a vector
#' @param yfix location, a vector
#' @param df a data frame created in \code{bk2trials}, a data frame
#' @usage vehfix(veh, nveh, ufix, xfix, yfix, df)
# #' @examples
# #' vehfix(veh, df)
#' @export
vehfix <- function(veh, nveh, ufix, xfix, yfix, df) {
  dfveh            <- as.data.frame(matrix(seq(1,6*nveh), byrow = TRUE, nrow = nveh))
  colnames(dfveh)  <-  c("t", "u", "x", "y", "lane", "vehicle")
  ucol      <- dfveh[veh,2]
  xcol      <- dfveh[veh,3]
  ycol      <- dfveh[veh,4]
  df[,ucol] <- ufix
  df[,xcol] <- xfix
  df[,ycol] <- yfix
  return(df)
}
