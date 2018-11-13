#' \code{simulate} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{simulate} runs \code{brktrials4wrapper} \code{size} times.
#' @param nveh1 number of vehicles entering the bottleneck from lane 1, a number
#' @param nveh2 number of vehicles entering the bottleneck from lane 2, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed standard deviation, a number
#' @param xstart1 start location of the first vehicle in lane 1, (feet), a number
#' @param xstart2 start location of the first vehicle in lane 2, (feet), a number
#' @param step time step, a number
#' @param tstart  vehicle crossovers are are permitted below this time, a number
#' @param tend upper time range of simulation, a number
#' @param xfunnel location where the lane drop is located, a number
#' @param leff vehicle length in feet, a number
#' @param type TRUE to create plots or FALSE otherwise, a logical
#' @param size sample size, a number
#' @usage simulate(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,size)
#' @export
simulate <- function(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,size) {
  input <- data.frame(nveh1,nveh2,umn,usd,xstart1,xstart2,
                      step,tstart,tend,xfunnel,leff,type,browse)
  print(input)
  for(run in 1:size) {
    if(run == 1) output <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,
                        step,tstart,tend,xfunnel,leff,type,browse) else {
                output. <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,
                        step,tstart,tend,xfunnel,leff,type,browse)
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
  browser()
  return(output)
}
