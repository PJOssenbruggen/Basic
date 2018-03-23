#' The \code{zipper} function simulates a bottleneck where two lanes of traffic merge
#' into one lane without causing any traffic delay.
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
zipper  <- function(tstart, tend,
                    ustart1, uend1, xstart1, xend1,
                    ustart2, uend2, xstart2, xend2,
                    ustart3, uend3, xstart3, xend3) {
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
    df1 <- rbind(df1, data.frame(i, t, u, x, Vehicle))
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
    df2 <- rbind(df2, data.frame(i, t, u, x, Vehicle))
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
    df3 <- rbind(df3, data.frame(i, t, u, x, Vehicle = "3"))
  }
  stp  <- dim(df1)[1]
  step <- c(seq(1,stp),seq(1,stp),seq(1,stp))
  o    <- order(step)
  step <- step[o]
  dfx  <- rbind(df1[,c(2,4,5)], df2[,c(2,4,5)], df3[,c(2,4,5)])
  dfu  <- rbind(df1[,c(2,3,5)], df2[,c(2,3,5)], df3[,c(2,3,5)])
  o    <- order(dfx[,1])
  dfx  <- dfx[o,]
  dfu  <- dfu[o,]
  #  plot(dfu[dfu[,3] == "1",1], dfu[dfu[,3] == "1",2], typ = "l", ylim = c(-25,90),xlab="t",ylab = "x")
  #  abline(h = 0)
  #  lines(dfu[dfu[,3] == "2",1], dfu[dfu[,3] == "2",2], col = gray(0.5))
  # lines(dfu[dfu[,3] == "3",1], dfu[dfu[,3] == "3",2], col = gray(0.8))
  if(a1 >= 0) dfperf1 <- data.frame(lane = 1, int.spd = ustart1, min.max.speed = max(dfu[dfu[,3] == "1",2]),
                                    time = dfu[dfu[,2] == max(dfu[dfu[,3] == "1",2]),1], a = a1, b = b1) else
                                      dfperf1 <- data.frame(lane = 1, int.spd = ustart1, min.max.speed = min(dfu[dfu[,3] == "1",2]),
                                                            time = dfu[dfu[,2] == min(dfu[dfu[,3] == "1",2]),1], a = a1, b = b1)
  if(a1 == 0) dfperf1 = dfperf1[1,]
  if(a2 >= 0) dfperf2 <- data.frame(lane = 2, int.spd = ustart2, min.max.speed = max(dfu[dfu[,3] == "2",2]),
                                    time = dfu[dfu[,2] == max(dfu[dfu[,3] == "2",2]),1], a = a2, b = b2) else
                                      dfperf2 <- data.frame(lane = 2, int.spd = ustart2, min.max.speed = min(dfu[dfu[,3] == "2",2]),
                                                            time = dfu[dfu[,2] == min(dfu[dfu[,3] == "2",2]),1], a = a1, b = b1)
  if(a2 == 0) dfperf2 = dfperf2[1,]
  if(a3 >= 0) dfperf3 <- data.frame(lane = 2, int.spd = ustart3, min.max.speed = max(dfu[dfu[,3] == "3",2]),
                                    time = dfu[dfu[,2] == max(dfu[dfu[,3] == "3",2]),1], a = a3, b = b3) else
                                      dfperf3 <- data.frame(lane = 2, int.spd = ustart3, min.max.speed = min(dfu[dfu[,3] == "3",2]),
                                                            time = dfu[dfu[,2] == min(dfu[dfu[,3] == "3",2]),1], a = a3, b = b3)
  if(a3 == 0) dfperf3 = dfperf3[1,]
  dfperf <- rbind(dfperf1, dfperf2, dfperf3)
#  print(dfperf)
  len  <- dim(dfx)[1]/3
  step <- c(seq(1,len), seq(1,len), seq(1,len))
  o    <- order(step)
  step <- step[o]
  dfx   <- cbind(dfx, step)
 # dfx   <- as.tibble(dfx)
  p    <- ggplot2::ggplot(dfx, ggplot2::aes(t, x, color = Vehicle, frame = step)) +
    ggplot2::geom_path(ggplot2::aes(cumulative = TRUE, group = Vehicle)) +
    ggplot2::ggtitle("Vehicles entering a Bottleneck")
  gganimate::gganimate(p, title_frame = FALSE)
}

