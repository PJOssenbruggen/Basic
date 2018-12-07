#' \code{zipper.simulate} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{zipper.simulate} runs \code{brktrials4wrapper} \code{size} times.
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
  xstart2 <- xstart1 - 0.5 * hsafe(umn*5280/3600,leff)
  runname <- "ZIPPER"
  input <- data.frame(runname, nveh1,nveh2,umn,usd,xstart1,xstart2,kfactor,
                      delt,tstart,tend,xfunnel,leff,sample.size = size)
  for(run in 1:size) {
    print(data.frame("Run:", run))
    if(run == 1) output <- brktrials4wrapper(
      nveh1,nveh2,umn,usd,xstart1,xstart2,delt,tstart,tend,xfunnel,leff,run,kfactor) else {
      output. <- brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,
                delt,tstart,tend,xfunnel,leff,run,kfactor)
                output  <- rbind(output, output.)
                                             }
  }
  sum.out1   <- data.frame(colMeans(output,na.rm = TRUE))
  sum.out2   <- data.frame(apply(output,2,sd,na.rm = TRUE))
  zipper.output <- output
  save(zipper.output, file = "/Users/PJO/Desktop/ZipperOutput.rda")
  zipper.summary<- cbind(sum.out1,sum.out2)
  colnames(zipper.summary) <- c("mean","SD")
  print(input)
  print(zipper.summary)
  spdl <- spdu <- {}
  k <- 0
  m <- 0
  for(i in 1:size) {
    if(output[i,2] >= 50) k <- k + 1
    if(output[i,2] <= 20) m <- m + 1
    if(output[i,2] >= 50) spdu <- c(spdu, output[i,2])
    if(output[i,2] <= 25) spdl <- c(spdl, output[i,2])
  }
  zipper.prop.free <- k/size
  zipper.prop.slow <- m/size
  save(zipper.summary, zipper.prop.free, zipper.prop.slow, file = "/Users/PJO/Desktop/ZipperPerformance.rda")
}
