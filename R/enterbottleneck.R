#' \code{enterbottleneck} produces a data frame that lists the order vehicles enter the bottleneck at location \code{xfunnel}..
#'
#' @return A list containing the table and number of vehicles \code{nveh}
#' @param lane1, a matrix
#' @param lane2, a matrix
#' @param xfunnel, a number
#' @param tend upper time range of simulation, a number
#' @param step time step, a number
#' @usage enterbottleneck(lane1,lane2, xfunnel, tend, step)
#' @export
enterbottleneck <- function(lane1, lane2, xfunnel, tend, step) {
  # order vehicles by the times that they reach x = 0
  nveh1     <- dim(lane1)[2]/3
  nveh2     <- dim(lane2)[2]/3
  veh1      <- seq(1,nveh1)
  veh2      <- seq(1,nveh2)
  id1       <- rep(1,nveh1)
  id2       <- rep(2,nveh2)
  tcrit1    <- tcrit3 <- rep(0, nveh1)
  tcrit2    <- tcrit4 <- rep(0, nveh2)
  tseq      <- seq(0, tend, step)
  tlen      <- length(tseq)
  for(veh in 1:nveh1) {
    xcol        <- 3*(veh-1) + 2
    tcrit1[veh] <- max(tseq[lane1[,xcol] <= 0])
    tcrit3[veh] <- max(tseq[lane1[,xcol] <= xfunnel])
  }
  # Establish tcrit2 range
  for(veh in 1:nveh2) {
    xcol        <- 3*(veh-1) + 2
    tcrit2[veh] <- max(tseq[lane2[,xcol] <= 0])
    tcrit4[veh] <- max(tseq[lane2[,xcol] <= xfunnel])
  }
  dfcrit1   <- data.frame(veh = veh1, tcrit = tcrit1, lane = id1)
  dfcrit2   <- data.frame(veh = veh2, tcrit = tcrit2, lane = id2)
  dfcrit3   <- data.frame(veh = veh1, tcrit = tcrit3, lane = id1)
  dfcrit4   <- data.frame(veh = veh2, tcrit = tcrit3, lane = id2)
  dfcrit    <- as.matrix(rbind(dfcrit1, dfcrit2))
  dfcrit5   <- as.matrix(rbind(dfcrit3, dfcrit4))
  dfcrit    <- cbind(dfcrit, dfcrit5)
  dfcrit    <- dfcrit[,-c(4,6)]
  nveh      <- nveh1 + nveh2
  t3        <- rep(tend,nveh)
  dfcrit    <- cbind(dfcrit,t3)
  colnames(dfcrit) <- c("veh", "t2", "lane", "t1","t3")
  o         <- order(dfcrit[,2])
  dfcrit    <- dfcrit[o,]
  dfcrit    <- dfcrit[,c(3,1,4,2,5)]
  rownames(dfcrit) <- paste("Merge.vehicle.",sep="",1:nveh)
  lead.t1   <- X01 <- X12 <- X23 <- rep(NA, dim(dfcrit)[1])
  dfcrit    <- cbind(dfcrit, X01,X12,X23,lead.t1)
  lead.1    <- lead.2 <- {}
  for(i in 1:nveh) if(dfcrit[i,1] == 1) lead.1 <- append(lead.1, i)
  for(i in 1:nveh) if(dfcrit[i,1] == 2) lead.2 <- append(lead.2, i)
  lead.1    <- c(0, lead.1[-length(lead.1)])
  lead.2    <- c(0, lead.2[-length(lead.2)])
  lead.1    <- as.matrix(lead.1)
  lead.2    <- as.matrix(lead.2)
  for(i in 1:nveh) {
    if(dfcrit[i,1] == 1) {
      dfcrit[i,9] <- lead.1[1]
      lead.1      <- lead.1[-1]
    } else {
      dfcrit[i,9] <- lead.2[1]
      lead.2      <- lead.2[-1]
    }
  }
  return(list(dfcrit, nveh))
  }
