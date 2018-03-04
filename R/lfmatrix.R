#' The function \code{lfmatrix} unlists the information \code{cflist}, the lists \code{L} and \code{F},
#' for lead and following vehicles, respectively.
#'
#' @param cflist information produced from the \code{run} and \code{trial} functions, a list.
#' @param nveh number of vehicles in the investigation or \code{vehicle} = {1, 2, ..., \code{nveh}}, a number
#' @return \code{cflist} returns two matrices consisting of speed \code{u} and location \code{x} data, which
#' are in turn are used to estimate performance measures for the simulation.
#' @usage lfmatrix(cflist, nveh)
lfmatrix <- function(cflist, nveh) {
  x0 <- x1 <- x2 <- x3 <- u0 <- u1 <- u2 <- u3 <- {}
  h0 <- h1 <- h2 <- h3 <- t1 <- t2 <- t3 <- tmp <- {}
  for(i in 1:nveh) {
    tmp <- unlist(cflist[[1]][i])
    t1  <- c(t1, tmp[5])
    t2  <- c(t2, tmp[9])
    t3  <- c(t3, tmp[13])
    x0  <- c(x0, tmp[3])
    x1  <- c(x1, tmp[7])
    x2  <- c(x2, tmp[11])
    x3  <- c(x3, tmp[15])
    u0  <- c(u0, tmp[2])
    u1  <- c(u1, tmp[6])
    u2  <- c(u2, tmp[10])
    u3  <- c(u3, tmp[14])
    h0  <- c(h0, tmp[4])
    h1  <- c(h1, tmp[8])
    h2  <- c(h2, tmp[12])
    h3  <- c(h3, tmp[16])
  }
  mat1 <- as.matrix(data.frame(t1, u1, x1, h1))
  mat0 <- as.matrix(data.frame(t0 = rep(0,nveh), u0, x0, h0))
  mat2 <- as.matrix(data.frame(t2, u2, x2, h2))
  mat3 <- as.matrix(data.frame(t3, u3, x3, h3))
  lst  <- list(mat0,mat1,mat2,mat3)
  return(lst)
}


