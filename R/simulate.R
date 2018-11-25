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
#' @param kfactor density at time \code{t} = 0, a number
#' @param browse TRUE to create plot or FALSE otherwise, a logical
#' @usage simulate(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,size,kfactor,browse)
#' @export
simulate <- function(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,size,kfactor,browse) {
  input <- data.frame(nveh1,nveh2,umn,usd,xstart1,xstart2,kfactor,
                      step,tstart,tend,xfunnel,leff,type,browse)
  for(run in 1:size) {
    print(data.frame("Run:", run))
    if(run == 1) output <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,
                        step,tstart,tend,xfunnel,leff,run,kfactor,browse) else {
                output. <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,
                        step,tstart,tend,xfunnel,leff,run,kfactor,browse)
                output  <- rbind(output, output.)
                }
  }
  sum.out1   <- data.frame(colMeans(output,na.rm=TRUE))
  sum.out2   <- data.frame(apply(output,2,sd,na.rm=TRUE))
  sum.out    <- cbind(sum.out1,sum.out2)
  colnames(sum.out) <- c("mean","SD")
  print(input)
  print(output)
  spdl <- spdu <- {}
  k <- 0
  m <- 0
  for(i in 1:size) {
    if(output[i,2] >= 50) k <- k + 1
    if(output[i,2] <= 20) m <- m + 1
    if(output[i,2] >= 50) spdu <- c(spdu, output[i,2])
    if(output[i,2] <= 25) spdl <- c(spdl, output[i,2])
  }
  proportions <- data.frame(nobreakdown.proportion = k/size, rare.event = m/size)
  return(list(input,sum.out,size,proportions,spdu,spdl))
}
