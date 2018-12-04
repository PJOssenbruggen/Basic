#' The \code{run} function is a wrapper for the \code{trial} and \code{plotfollow3} functions.
#'
#' @param nveh number of vehicles in the investigation or \code{vehicle} = {1, 2, ..., \code{nveh}}, a number
#' @param ulead lead vehicle speed (mph) at time \code{t0}, i.e., before breakdown, a number
#' @param ulead.sd standard deviation of \code{ulead}, a number
#' @param ubrkdown breakdown speed (mph) of lead vehicle, a number
#' @param ubrkdown.sd standard deviation of \code{ubrkdown}, a number
#' @param k traffic density (vehicles per mile, vpm) of the pre-breakdown state, a number
#' @param k.sd standard deviation of following vehicle breakdown speed, a number
#' @param d deceleration rate of the lead  \code{l} vehicle, a number
#' @param leff effective vehicle length (feet), a number
#' @param xlim plot range along the \code{t} axis, a vector
#' @param ylim plot range along the \code{x} axis, a vector
#' @return The function returns an \code{t-x} image and eight lists:
#'  \code{output[[1]]} returns a vector of \code{run} input values; \code{output[[2]]} returns
#'  a matrix of pre-breakdown \code{ulead} speed, breakdown \code{ubrkdown} speed,
#'  pre-breakdown \code{k} density values for each vehicle; \code{output[[3]]} returns
#'  a time matrix consisting of \code{t0,t1,t2,t3,t4,t5};
#'  \code{output[[4]], output[[5]], output[[6]]} and \code{output[[7]]} returns matrices
#'  of speed \code{u}, location {x} and safe headway \code{h} values for each vehicle
#'  at times \code{t0,t1,t2,t3}, respectively. Finally, \code{output[[8]]} returns a list
#'  the \code{cflist} list consisting of information stored as \code{L} and \code{Fw} matrices.
#' @export
run <- function(nveh, ulead, ulead.sd, ubrkdown, ubrkdown.sd, k, k.sd, d, leff, xlim, ylim) {
  input <- as.matrix(data.frame(nveh, ulead, ulead.sd, ubrkdown, ubrkdown.sd, k, k.sd, d, leff))
  ul    <- c(ulead, rnorm(n = nveh - 1, mean = ulead,  sd = ulead.sd))
  if(ubrkdown == 0) ub <- rep(0, nveh) else {
    ub..<- ubreakdown(100 * nveh, ubrkdown, ubrkdown.sd)
    ub. <- sample(ub.., 5 * nveh)
    o   <- order(ub., decreasing = TRUE)
    ub. <- ub.[o]
    ub  <- {}
    for(i in 1:length(ub.)) if(ub.[i] < 50) ub <- append(ub, ub.[i])
    ub  <- ub[1:nveh]
  }
  kf    <- rnorm(n = nveh, mean = k,sd = k.sd)
  vehchar <- as.matrix(data.frame(vehicle = seq(1,nveh), ul, ub, kf))
  ul  <- vehchar[1,2]
  ubk <- vehchar[1,3]
  k   <- vehchar[1,4]
  uf  <- vehchar[1,2]
  cflist <- trial(ul, ubk, k, uf, d, leff, xlim, ylim)
# trajectories for the next vehicles.
  t4 <- xlim[2]
#  if(nveh == 1) return(cflist)

  for(i in 2:nveh) {
    if(nveh == i) {
      times  <- cflist[[2]]
      mats   <- lfmatrix(cflist = cflist, nveh = nveh)
      mat0   <- cbind(vehicle = seq(1,nveh), mats[[1]])
      mat1   <- cbind(vehicle = seq(1,nveh), mats[[2]])
      mat2   <- cbind(vehicle = seq(1,nveh), mats[[3]])
      mat3   <- cbind(vehicle = seq(1,nveh), mats[[4]])
      output <- list(input, vehchar, times, mat0, mat1, mat2, mat3, cflist)
      return(output)
    } else {
      uf  <- vehchar[i,2]
      ub  <- vehchar[i,3]
      kf  <- vehchar[i,4]
      cflist <- plotfollow3(cflist, vehicle = i, uf, kf, ub, leff, t4)
    }
  }
  return(cflist)
}
