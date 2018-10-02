#' \code{mergedata} produces a data frame that lists the order vehicles enter the bottleneck at location \code{xfunnel}..
#'
#' @return \code{df1df2} is a matrix of both lane 1 and 2.
#' @param lane1, a matrix
#' @param lane2, a matrix
#' @param tlen number of time steps, a number
#' @param dfcrit list of information about lanes and vehicles, a matrix
#' @usage mergedata(lane1,lane2, tlen, dfcrit)
#' @export
mergedata <- function(lane1, lane2, tlen, dfcrit) {
  # Set up df matrix by combining lane 1 and 2 matrices. Call it df1df2 where df1 and df2 are ordered as shown in dfcrit
  nveh1         <- dim(lane1)[2]/3
  nveh2         <- dim(lane2)[2]/3
  nveh          <- nveh1 + nveh2
  df1df2        <- matrix(rep(0,3*nveh*tlen),ncol=3*nveh)
  df1df2id      <- matrix(seq(1,3*nveh), ncol = 3, byrow = TRUE)
  lane1id       <- matrix(seq(1,3*nveh1), ncol = 3, byrow = TRUE)
  lane2id       <- matrix(seq(1,3*nveh2), ncol = 3, byrow = TRUE)
  colnames(df1df2id) <- colnames(lane1id) <- colnames(lane2id) <- c("u","x","y")
  rownames(df1df2id) <- paste("Merge.vehicle.",sep="",seq(1,nveh))
  rownames(lane1id)  <- paste("Merge.vehicle.",sep="",seq(1,nveh1))
  rownames(lane2id)  <- paste("Merge.vehicle.",sep="",seq(1,nveh2))
  dfcrit        <- dfcrit[,c(2,4,1,3,5,6,7,8,9)]
  dfcrit        <- cbind(dfcrit, df1df2id)
  for(veh in 1:nveh) {
    if(dfcrit[veh,3] == 1) {
      df1df2[,dfcrit[veh,10]] <- lane1[,lane1id[dfcrit[veh,1],1]]
      df1df2[,dfcrit[veh,11]] <- lane1[,lane1id[dfcrit[veh,1],2]]
      df1df2[,dfcrit[veh,12]] <- lane1[,lane1id[dfcrit[veh,1],3]]
    } else {
      df1df2[,dfcrit[veh,10]] <- lane2[,lane2id[dfcrit[veh,1],1]]
      df1df2[,dfcrit[veh,11]] <- lane2[,lane2id[dfcrit[veh,1],2]]
      df1df2[,dfcrit[veh,12]] <- lane2[,lane2id[dfcrit[veh,1],3]]
    }
  }
  names <- {}
  for(veh in 1:nveh) {
    if(dfcrit[veh,3] == 1) {
        seg   <- paste("1.",sep="",dfcrit[veh,1])
        uname <- paste("u.",sep="",seg)
        seg   <- paste("1.",sep="",dfcrit[veh,1])
        xname <- paste("x.",sep="",seg)
        seg   <- paste("1.",sep="",dfcrit[veh,1])
        yname <- paste("y.",sep="",seg)
        names <- c(names, uname, xname, yname)
      } else {
      seg   <- paste("2.",sep="",dfcrit[veh,1])
      uname <- paste("u.",sep="",seg)
      seg   <- paste("2.",sep="",dfcrit[veh,1])
      xname <- paste("x.",sep="",seg)
      seg   <- paste("2.",sep="",dfcrit[veh,1])
      yname <- paste("y.",sep="",seg)
      names <- c(names, uname, xname, yname)
      }
  }
  colnames(df1df2) <- names
  return(df1df2)
}
