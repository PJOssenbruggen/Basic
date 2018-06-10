#' \code{animate_rr} draws the path around the ring-road
#' using data \code{LF} from \code{rrtrials}.
#'
#' @param LF, a matrix
#' @param leff, effective vehicle length (feet), number
#' @usage animate_rr(LF, leff)
animate_rr <- function(LF, leff) {
  n   <- 20                 # n = 20 gives a reasonable looking animation plot.
  k0  <- LF[[1]][3]
  umn <- LF[[1]][1]
  r <- n/(2*pi*k0) * 5280  # 373.48 feet (radius)
  c <- 2 * pi * r          # c = circumference (feet)
# z = locations of lead vehicle in 60 one-second intervals.
# Assume constant speed
  t  <- seq(0,LF[[1]][4])
  z1 <- umn * t                 # umn = uniform speed
  z2 <- z1 - hsafe(umn, leff)   # following vehicle keeps a safe headway
  theta1 <- (2*pi/5280) * z1
  theta2 <- (2*pi/5280) * z2
  x1 <- cos(theta1) * r
  y1 <- sin(theta1) * r
  x2 <- cos(theta2) * r
  y2 <- sin(theta2) * r
  veh1 <- rep("Leader", length(t))
  veh2 <- rep("Follower", length(t))
  x <- c(x1, x2)
  y <- c(y1, y2)
  z <- c(z1, z2)
  veh <- c(veh1, veh2)
  data1 <- data.frame(t, x, y, z, Vehicle = veh)
  data <- dplyr::as_tibble(data1)
  p1 <- ggplot2::ggplot(data, ggplot2::aes_string("x", "y", frame = "t", color = "Vehicle")) +
    ggplot2::geom_point(ggplot2::aes_string()) +
    ggplot2::ggtitle("Flow-Speed Plot")

#  Replace umn with stochastic speed
  data2 <- data1
  u1 <- LF[[3]][,2]
  u2 <- LF[[3]][,3]
  z1 <- LF[[2]][,2]
  z2 <- LF[[2]][,3]
  theta1 <- (2*pi/5280) * z1
  theta2 <- (2*pi/5280) * z2
  x1 <- cos(theta1) * r
  y1 <- sin(theta1) * r
  x2 <- cos(theta2) * r
  y2 <- sin(theta2) * r
  veh1 <- rep("Leader", length(t))
  veh2 <- rep("Follower", length(t))
  x <- c(x1, x2)
  y <- c(y1, y2)
  z <- c(z1, z2)
  headway <- data.frame(z.lead = z1, z.follow = z2, h = z1 - z2)
  veh <- c(veh1, veh2)
  data2[,2] <- x
  data2[,3] <- y
  data3 <-
  data <- dplyr::as_tibble(data2)
  p2 <- ggplot2::ggplot(data, ggplot2::aes_string("x", "y", frame = "t", color = "Vehicle")) +
    ggplot2::geom_point(ggplot2::aes_string()) +
    ggplot2::ggtitle("Flow-Speed Plot")
  return(list(p1,p2,data3))
}
