#' \code{dfmanager} is a wrapper function for \code{brktrials2}
#'
#' @return \code{dfmanager} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
#' @param nveh number of vehicles in the simulation, a number
#' @param tend end time for a simulation run, a number
#' @param umn start speed (mph) for vehicle in lane 1, a number
#' @param usd speed volatility for \code{umn}, a number
#' @param xstart a vector of  \code{nveh} start locations, (feet), a vector
#' @param xfunnel upstream location where the lane drop starts (feet), a number
#' @param leff effective vehicle length(feet), a number
#' @param lane a vector of \code{nveh} numbers, a vector
#' @param step size in seconds, a number
#' @usage dfmanager(nveh, tend, umn, usd, xstart, xfunnel, leff, lane, step)
# #' @examples
# #' dfmanager(nveh = 3, tend = 16, umn = 41, usd = 11, xstart = c(-600,-750,-1000), xfunnel = -500, leff = 14, lane = c(1,2,1), step = 2)
#' @export
dfmanager <- function(nveh, tend, umn, usd, xstart, xfunnel, leff, lane, step) {
  set.seed(127)
  tend.save <- tend
  tseq <- seq(0,tend,step)
  tlen <- length(tseq)
  lane.<- lane
  tstart <- 0
  for(veh in 1:nveh) {
    df.     <- bmfree2(umn, usd, tstart, tend, xstart[veh], step)
    vehicle <- rep(veh, tlen)
    y       <- rep(0, tlen)
    lane    <- rep(lane.[veh], tlen)
    df.     <- cbind(df., y, lane, vehicle)
    if(veh == 1) df <- df. else df <- cbind(df, df.)
  }
  tstart. <- 6
  tend.   <- 12
  tseq    <- seq(tstart., tend., step)
  ufix    <- rep(10, length(tseq))
  xfix    <- rep(100, length(tseq))
  veh <- 2
  df <- vehtfix(veh, nveh, df, tstart., tend., step, ufix, xfix)
  return(df)
}
