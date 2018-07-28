#' \code{vehid} selects information about vehicle \code{veh}
#' from the \code{tuxv} matrix for analysis with the \code{brktrials3} function.
#'
#' @param veh vehicle, a number
#' @param tuxv a data frame created in\code{brktrials3}, a matrix
#' @usage vehid(veh, tuxv)
#' @export
# #' @examples
# #' vehid(veh, tuxv)
vehid <- function(veh, tuxv) {
  nveh        <- (dim(tuxv)[2] - 1)/3
  vehseq      <- seq(2,3*nveh+1)
  df          <- as.data.frame(matrix(vehseq, byrow = TRUE, nrow = nveh))
  colnames(df)<- c("u", "x", "y")
  dfid        <- as.numeric(df[veh,])
  dfij        <- tuxv[,dfid]
  dfij        <- cbind(t = tuxv[,1],dfij)
  return(dfij)
}
