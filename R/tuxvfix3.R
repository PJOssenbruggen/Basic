#' \code{tuxvfix3} fixes \code{hsafe} violations for vehicle \code{veh} in matrix \code{tuxv}.
#'
#' @param i index, anumber
#' @param vehorder vehicle, a vector
#' @param nveh number of vehicle in \code{df}, a number
#' @param tuxv a data frame created in\code{bk2trials}, a matrix
#' @param ufix new speeds, a vector
#' @param xfix new locations, a vector
#' @usage tuxvfix3(i, vehorder, nveh, tuxv, ufix, xfix)
#' @export
# #' @examples
# #' tuxvfix3(veh, df)
tuxvfix3 <- function(i, vehorder, nveh, tuxv, ufix, xfix) {
  veh    <- vehorder[i]
  rid    <- seq(1,nveh*3) + rep(1,nveh*3)
  vehid  <- matrix(rid, ncol = 3, byrow = TRUE)
  vseq   <- seq(1,nveh)
  vehid  <- cbind(vseq, vehid)
  colnames(vehid) <- c("vehicle", "u", "x", "y")
  ucol   <- as.numeric(vehid[veh,2])
  tuxv[, ucol] <- ufix
  xcol   <- as.numeric(vehid[veh,3])
  tuxv[,xcol] <- xfix
  return(tuxv)
}
