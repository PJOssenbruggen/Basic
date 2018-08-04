#' The function \code{trial} produces traffic performance information, stored in lists \code{L} and \code{Fw}
#' for lead and following vehicles, respectively/
#' \code{L} and \code{Fw} contain the information needed to graph a time-distance \code{t-x} trajectories for two vehicles.
#'
#' @param ul pre-breakdown speed of lead \code{l} vehicle, a number
#' @param ubk breakdown speed of lead \code{l} vehicle, a number
#' @param uf pre-breakdown speed  of following \code{f} vehicle, a number
#' @param k traffic density  of the pre-breakdown state, a number
#' @param d deceleration rate of the first of lead  \code{l} vehicle, a number
#' @param leff effective vehicle length (feet), a number
#' @param xlim plot range along the \code{t} axis, a vector
#' @param ylim plot range along the \code{x} axis, a vector
#' @return \code{LF} consists of a list of four outputs: (1) an input vector where speed is given
#' in miles per hour (mph), where vehicle density is given in vehicles per mile, vpm,
#' where deceleration rate is given in feet per second squared (fps2), and where vehicle length in feet; (2) an
#' input vector using feet and second scales exclusively; a car-following matrix \code{L}; and
#' (4) a \code{output} vector consisting various performance estimates of delay including traffic flow in
#' the congested state and shock-wave speed for evaluating queuing.
#' @usage trial(ul, ubk, k, uf, d, leff, xlim, ylim)
# #' @examples
# #' trial(63, 10, 55, 70, 15, 14, c(0,8), c(-500, 60))
# #' trial(63, 34, 22, 70, 15, 14, c(0,12), c(-1500, 600))
# #' trial(63, 34, 55, 70, 15, 14, c(0,12), c(-1500, 600))
trial <- function(ul, ubk, k, uf, d, leff, xlim, ylim) {
# Transformed to fps scale.
#  print(data.frame(ul, ubk, k, uf, d, leff))
  input  <- as.matrix(data.frame(ul, ubk, k, uf, d, leff))
  ul0    <- as.numeric(ul)   * 5280 / 3600
  ul3    <- as.numeric(ubk)  * 5280 / 3600
  uf0    <- as.numeric(uf)   * 5280 / 3600
  k      <- as.numeric(k)    / 5280
# print(data.frame(ul0, ul3, k, uf0, d, leff))
# t3 is the time the lead vehicle decelerates and obtains speed ul3.
  t3     <- (ul0 - ul3) / d
  upre   <- ul0
  u0     <- ul3
  theta  <- gbmtheta(upre, u0, t3)
  lead   <- matrix(c(ul0, ul3, k, uf0, d, t3, leff, theta), nrow = 1)
  colnames(lead) <- c("ul0","ul3","k","uf0","d", "t3","leff","theta")
  t0     <- 0
  xl3    <- 0
  u0     <- ul0
  xl0    <- -1 * gbmx(u0, theta, t = t3)
# Note: two lanes merge into one lane.
  h0     <- as.numeric(2 / k)
  xf0    <- xl0 - h0
  h3     <- hsafe(ul3, leff)
# plots lead vehicle trajectory
  L <- matrix(rep(NA, 16), ncol = 4)
  colnames(L) <- c("t0","t1","t2","t3")
  rownames(L) <- c("t", "u","x","h")
  L[1,1] <- t0;   L[1,2] <- NA;   L[1,3] <- NA;  L[1,4] <- t3;
  L[2,1] <- ul0;  L[2,2] <- NA;   L[2,3] <- NA;  L[2,4] <- ul3
  L[3,1] <- xl0;  L[3,2] <- NA;   L[3,3] <- NA;  L[3,4] <- xl3
  L[4,1] <- h0;   L[4,2] <- NA;   L[4,3] <- NA;  L[4,4] <- h3
  plotlead(L, theta, leff, xlim, ylim)
#  print("trial")
#  print(data.frame(vehicle = 1))
#  print("All information in matrix L is restricted to the lead vehicle and how it changes speed from ul0 to ul3.")
#  print("L = ")
  Fw <- matrix(rep(NA, 16), ncol = 4)
  colnames(Fw) <- c("t0","t1","t2","t3")
  rownames(Fw) <- c("t", "u", "x", "h")
#  xl0 <- as.numeric(xl0); xf0 <- as.numeric(xf0); ul0 <- as.numeric(ul0);
#  uf0 <- as.numeric(xf0); theta <- as.numeric(theta);
#  print("Sight line and finding t2: t2 is a response time where the following vehicle driver")
#  print("sees the location and speed of the lead vehicle and responds accordingly.")
#  print(c(xl0, xf0, ul0, uf0, theta, leff))
# Find t1
# t1 is the time when the following vehicle responds to the deceleration of the lead vehicle.
  answer1 <- findt1(xl0, xf0, ul0, uf0, theta, leff)
  t1     <- answer1[1]
  uf1    <- answer1[3]
  h1     <- hsafe(uf1, leff)
  xf1.   <- answer1[2]
  xf1    <- xf1.
  t4     <- 100
#  points(t1, xf1, pch = 8, cex = 0.5)
  # Find t2
  answer2<- findt2(L, theta, t4)
  t2     <- answer2[1]
  ul2    <- answer2[2]
  xl2    <- answer2[3]
  h2     <- hsafe(ul2, leff)
  xf2    <- xl2 - h2
  h3     <- hsafe(ul3, leff)
  xf3    <- xl3 - h3
# Prevents vehicle from traveling backwards
# points(t2, xf2, pch = 15, cex = 0.5)
  Fw[1,1] <- t0;   Fw[1,2] <- t1;   Fw[1,3] <- t2;   Fw[1,4] <- t3;
  Fw[2,1] <- uf0;  Fw[2,2] <- uf1;  Fw[2,3] <- ul2;  Fw[2,4] <- ul3
  Fw[3,1] <- xf0;  Fw[3,2] <- xf1;  Fw[3,3] <- xf2;  Fw[3,4] <- xf3
  Fw[4,1] <- h0;   Fw[4,2] <- h1;   Fw[4,3] <- h2;   Fw[4,4] <- h3
#  print("Finding t1: t1 is dependent on the lead, following vehicle speeds and headway h0.")
#  print("The values of h0, h1, h2 and h3, the safe headways at time t0, t1, t2 and t3, are")
#  print("functions of their lead vehicle speeds at these times.")
  t4     <- xlim[2]
  output <- plotfollow(L, Fw, t4)
  LF     <- list(L, Fw)
  output <- as.matrix(output)

  return(list(LF, output))
}
