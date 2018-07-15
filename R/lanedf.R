#' \code{lanedf} returns a vector of vehicles \code{veh} in \code{lane}.
#'
#' @param pick a lane number
#' @param nveh, a number
#' @param lane, a vector
#' @usage lanedf(pick, nveh, lane)
lanedf <- function(pick, nveh, lane) {
  updf <- data.frame(veh = seq(1,nveh), lane = lane)
  df   <- updf[updf[,2] == pick,]
  return(df)
}
