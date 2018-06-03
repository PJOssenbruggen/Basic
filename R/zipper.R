#' The \code{zipper} function can be used to simulate interaction among three vehicles in car following
#' on a single lane or three vehicles merging on a two-lane highway.
#'
#' @param tstart start time, a number
#' @param tend end time, a number
#' @param ustart1 start speed (mph) for vehicle in lane 1, a number
#' @param uend1 end speed (mph) for vehicle in lane 1, a number
#' @param xstart1 start location for vehicle in lane 1, a number
#' @param xend1 end location for vehicle in lane 1, a number
#' @param ustart2 start speed (mph) for vehicle in lane 2, a number
#' @param uend2 end speed (mph) for vehicle in lane 2, a number
#' @param xstart2 start location for vehicle in lane 2, a number
#' @param xend2 end location for vehicle in lane 2, a number
#' @param ustart3 start speed (mph) for vehicle in lane 3, a number
#' @param uend3 end speed (mph) for vehicle in lane 3, a number
#' @param xstart3 start location for vehicle in lane 3, a number
#' @param xend3 end location for vehicle in lane 3, a number
#' @return \code{zipper} uses a deterministic model and animation to illustrate an ``idealistic'' situtaion,
#' a so-called a ``zipper merge.''
#' @usage zipper(tstart, tend,
#'     ustart1, uend1, xstart1, xend1,
#'     ustart2, uend2, xstart2, xend2,
#'     ustart3, uend3, xstart3, xend3)
#' @examples
#' zipper(0,60,90,90,0,5000,90,90,-200,4500,90,90,-500, 4000)
#' zipper(0,40,85,90,0,4000,90,99,0,4500,90,90,-500, 4200)
zipper  <- function(tstart, tend,
                    ustart1, uend1, xstart1, xend1,
                    ustart2, uend2, xstart2, xend2,
                    ustart3, uend3, xstart3, xend3
                    )
  {
  tseq   <- seq(0, tend, by = 0.2)
  xfseq1 <- ufseq1 <- xlseq1 <- ulseq1 <- {}
  xfseq2 <- ufseq2 <- xlseq2 <- ulseq2 <- {}
  xfseq3 <- ufseq3 <- xlseq3 <- ulseq3 <- {}
  dfab   <- xabparam(tstart, tend, ustart = ustart1, uend = uend1, xstart = xstart1, xend = xend1)
  a1     <- dfab[1]
  b1     <- dfab[2]
  u0    <- ustart1
  x0    <- xstart1
  t0    <- 0
  df1   <- df2 <- df3 <- {}
  for(i in 1:length(tseq)) {
    t   <- tseq[i]
    u   <- uab(u0,a = a1, b = b1,t,t0)
    x   <- xab(x0,u0,a = a1, b = b1,t,t0)
    Vehicle = "1"
    df1 <- rbind(df1, data.frame(t, u, x, Vehicle))
  }
  dfab  <- xabparam(tstart,tend,ustart = ustart2, uend = uend2, xstart = xstart2, xend = xend2)
  a2    <- dfab[1]
  b2    <- dfab[2]
  u0    <- ustart2
  x0    <- xstart2
  t0    <- 0
  for(i in 1:length(tseq)) {
    t   <- tseq[i]
    u   <- uab(u0,a = a2, b = b2,t,t0)
    x   <- xab(x0,u0,a = a2, b= b2,t,t0)
    Vehicle = "2"
    df2 <- rbind(df2, data.frame(t, u, x, Vehicle))
  }
  dfab  <- xabparam(tstart,tend,ustart = ustart3, uend = uend3, xstart = xstart3, xend = xend3)
  a3    <- dfab[1]
  b3    <- dfab[2]
  u0    <- ustart3
  x0    <- xstart3
  t0    <- 0
  for(i in 1:length(tseq)) {
    t   <- tseq[i]
    u   <- uab(u0,a = a3, b = b3,t,t0)
    x   <- xab(x0,u0,a = a3, b = b3,t,t0)
    df3 <- rbind(df3, data.frame(t, u, x, Vehicle = "3"))
  }
  df = rbind(df1, df2, df3)
  p   <- ggplot2::ggplot(df, ggplot2::aes(t, x, colour = Vehicle, size = u, frame = t)) +
    ggplot2::geom_point()
  return(p)
}

