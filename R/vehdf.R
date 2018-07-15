#' \code{vehdf} selects information about vehicle \code{veh}
#' of the \code{df} matrix for analysis with the \code{brktrials2} function.
#'
#' @param veh vehicle, a number
#' @param nveh number of vehicle in \code{df}, a number
#' @param df a data frame created in\code{bk2trials}, a data frame
#' @usage vehdf(veh, nveh, df)
#' @export
# #' @examples
# #' vehdf(veh, nveh, df)
vehdf <- function(veh, nveh, df) {
  dfcol         <- as.data.frame(matrix(seq(1,6*nveh), byrow = TRUE, nrow = nveh))
  colnames(dfcol)<- c("t", "u", "x", "y", "lane", "vehicle")
  dfcol         <- as.numeric(dfcol[veh,])
  dfij          <- df[,dfcol]
  return(dfij)
}
