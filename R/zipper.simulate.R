#' \code{zipper.simulate} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{zipper.simulate} returns a list of output.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed standard deviation, a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param delt time-step, a number
#' @param tstart  vehicle crossovers are are permitted below this time, a number
#' @param tend upper time range of simulation, a number
#' @param xfunnel location where the lane drop is located, a number
#' @param leff vehicle length in feet, a number
#' @param size sample size, a number
#' @param kfactor density at time \code{t} = 0, a number
#' @usage zipper.simulate(nveh1,nveh2,umn,usd,xstart1,delt,tstart,tend,xfunnel,leff,size,kfactor)
#' @export
zipper.simulate <- function(nveh1,nveh2,umn,usd,xstart1,delt,tstart,tend,xfunnel,leff,size,kfactor) {
  set.seed(123)
  xstart2 <- xstart1 - 0.5 * hsafe(umn*5280/3600,leff)
  runname <- "ZIPPER"
  input   <- data.frame(runname, nveh1,nveh2,umn,usd,xstart1,xstart2,kfactor,
                      delt,tstart,tend,xfunnel,leff,sample.size = size)
  print(input)
  for(run in 1:size) {
    print(data.frame("Run:", run))
    if(run == 1) {
      lst    <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,delt,
                                  tstart,tend,xfunnel,leff,run,kfactor)
      output <- lst[[1]]
      kq     <- lst[[2]]
      tdf1df2<- lst[[3]]
      save(tdf1df2, file = "/Users/PJO/Desktop/Zipper/tdf1df2.rda")
    } else {
      lst    <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,delt,
                                  tstart,tend,xfunnel,leff,run,kfactor)
      output. <- lst[[1]]
      kq.     <- lst[[2]]
      output  <- rbind(output, output.)
      kq      <- rbind(kq, kq.)
    }
  }
  sum.out1   <- data.frame(colMeans(output,na.rm = TRUE))
  sum.out2   <- data.frame(apply(output,2,sd,na.rm = TRUE))
  summary    <- cbind(sum.out1,sum.out2)
  colnames(summary) <- c("mean","SD")
  print(input)
  print(summary)
  spdl <- spdu <- {}
  k    <- 0
  m    <- 0
  for(i in 1:size) {
    if(output[i,2] >= 50) k <- k + 1
    if(output[i,2] <= 20) m <- m + 1
    if(output[i,2] >= 50) spdu <- c(spdu, output[i,2])
    if(output[i,2] <= 25) spdl <- c(spdl, output[i,2])
  }
  prop.free <- k/size
  prop.slow <- m/size
  return(list(input,summary,output, prop.free, prop.slow, kq[,c(1,4,5,9,12)]))
}
