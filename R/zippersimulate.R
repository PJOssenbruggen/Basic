#' \code{zippersimulate} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{zippersimulate} runs \code{zipper3wrapper} \code{size} times.
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
#' @usage zippersimulate(nveh1,nveh2,umn,usd,xstart1,delt,tstart,tend,xfunnel,leff,size,kfactor)
#' @export
zippersimulate <- function(nveh1,nveh2,umn,usd,xstart1,delt,tstart,tend,xfunnel,leff,size,kfactor) {
  xstart2 <- xstart1 - hsafe(umn*5280/3600,leff) / 2
  runname <- "Zipper"
  input <- data.frame(runname,nveh1,nveh2,umn,usd,xstart1,xstart2,xfunnel,leff,kfactor,sample.size = size)
  print(input)
  for(run in 1:size) {
    print(data.frame("Run:", run))
    if(run == 1) output <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,delt,
                             tstart,tend,xfunnel,leff,run,kfactor) else {
                             output. <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,delt,
                                  tstart,tend,xfunnel,leff,run,kfactor)
                             output  <- rbind(output, output.)
                             }
  }
  print(output)
  outputruns <- rbind(outputruns, output)
  sum.out1   <- data.frame(colMeans(outputruns,na.rm=TRUE))
  sum.out2   <- data.frame(apply(outputruns,2,sd,na.rm=TRUE))
  sum.out    <- cbind(sum.out1,sum.out2)
  colnames(sum.out) <- c("mean","SD")
  print(sum.out)
  return(output)
}
