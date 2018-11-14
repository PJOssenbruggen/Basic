#' \code{zipperwrapper} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
#'
#' @return The \code{zipperwrapper}, a wrapper function for \code{bmfree3}, \code{xabparam} and
#' \code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory
#' for the following vehicle, and critical information. The lead vehicle trajectory is not affected. Passing is not permitted.
#' The input matrix \code{lane} contains desire lines for a leading and following vehicle.
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
#' @param browse to inspect \code{fixviolation} to inspect plot or FALSE otherwise
#' @usage zipperwrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse)
# #' @examples
# #' zipperwrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse)
#' @export
zipperwrapper  <- function(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse) {
  #### STEP 1. Create lane 1 and 2 datasets ################################################################
  #set.seed(123)
  set.seed(403)
  #set.seed(333)
  xlim  <- c(tstart,tend)
  print(data.frame(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,type,browse))
  df1df2 <- zippermerge(nveh, tstart, tend, xstart1, umn, leff, xfunnel, step, type)
  print(df1df2[1:10,])
#  browser()
  #### STEP 2. Order vehicles by the times that they reach x = 0 ####################
  index    <- matrix(seq(2,dim(df1df2)[2]),ncol = 3, byrow = TRUE)
  index    <- cbind(veh = seq(1,nveh), index)
  lane1veh <- seq(1,nveh,2)
  index1   <- as.vector(index[lane1veh,2:4])
  o        <- order(index1)
  index1   <- index1[o]
  lane1    <- df1df2[,index1]
  lane2veh <- seq(2,nveh,2)
  index2   <- as.vector(index[lane2veh,2:4])
  o        <- order(index2)
  index2   <- index2[o]
  lane2    <- df1df2[,index2]
  lst      <- enterbottleneck(lane1,lane2,xfunnel,tend,step)
  dfcrit   <- lst[[1]]
  nveh     <- lst[[2]]
  print(dfcrit)

  #### STEP 3. Merge lane 1 and 2 data sets ##########################################
  tend.0 <- tend
  tseq   <- seq(0, tend, step)
  tlen   <- length(tseq)
  df1df2 <- mergedata(lane1,lane2,tlen,dfcrit)
  # Zone violations
  for(veh in 1:(nveh)) {
    for(zone in 1:3) {
      dfcrit <- zoneviolation(veh, zone, df1df2, dfcrit, step, tend.0, leff)
    }
  }
  print(dfcrit)

  #### STEP 6. Flow Estimation ######################################################
  lst <- flow(df1df2, tstart, tend, step, xfunnel,TRUE)
  df1 <- lst[[1]]
  df2 <- lst[[2]]
  if(type == TRUE) {
    par(mfrow = c(1,2), pty = "s")
    plot(df1[,2], df1[,1], typ = "s", xlim = c(0,max(df1[,3])), ylim = c(1,nveh+0.1),
         ylab = "Vehicle", xlab = "t, seconds", col = "red", lwd = 2)
    lines(df1[,3], df1[,1], typ = "s", col = "orange", lwd = 2)
    abline(v = 0, col = gray(0.8))
    abline(h = 1, col = gray(0.8))
    sub = ""
    if(usd == 0) sub <- "Side-by-side merge." else sub <- "Unmanaged zipper merge."
    title(main = "Bottleneck", sub)
   # axis(side = 3, at = max(df1[,3])/2, sub, tick = FALSE, line = -1)
    legend("topleft", legend = c("A = arrival","D = departure"), lty = c(1,1), col = c("red","orange"), bty = "n")
    u.a.mean.mph <- as.numeric(df2[1])
    u.d.mean.mph <- as.numeric(df2[2])
  }
  #### STEP 7. Capacity Estimation
  df1    <- flow2(dfcrit, df1df2, tstart, tend, step, xfunnel,TRUE)
  tservice = abs(round(xfunnel/(5280/3600*umn),1))
  if(type == TRUE) {
    xlim <- c(0, 200)
    ylim <- c(0, 4000)
    plot(df1[,7], df1[,8], typ = "p", xlim, ylim, ylab = "q, vph",
         xlab = "k, vpm", pch = 16, cex = 1, col = "orange")
    points(df1[,10],df1[,11], pch = 16, col = "red")
    sub = expression("Service time: "*W(t) == D(t) - A(t))
    title(main = "Bottleneck" , sub)
    k.d.mn <- round(mean(df1[,7], na.rm = TRUE),0)
    q.d.mn <- round(mean(df1[,8], na.rm = TRUE),0)
    k.a.mn <- round(mean(df1[,10], na.rm = TRUE),0)
    q.a.mn <- round(mean(df1[,11], na.rm = TRUE),0)
    w.mn   <- round(mean(df1[,6], na.rm = TRUE),1)
    axis(side = 3, at = k.a.mn, labels = expression(bar(k)[A]), tick = FALSE, line = -1)
    axis(side = 3, at = k.d.mn, labels = expression(bar(k)[D]), tick = FALSE, line = -1)
    abline(h = 0, col = gray(0.8))
    abline(v = 0, col = gray(0.8))
    abline(h = q.a.mn, lty = 2, col = "orange")
    abline(h = q.d.mn, lty = 2, col = "red")
    abline(v = k.a.mn, lty = 2,col= "orange")
    abline(v = k.d.mn, lty = 2,col= "red")
    axis(side = 4, at = q.d.mn, labels = expression(bar(q)[D]), tick = FALSE, line = -0.75)
    axis(side = 4, at = q.a.mn, labels = expression(bar(q)[A]), tick = FALSE, line = -0.75)
    u.a.mean.mph <- round(mean(as.numeric(df1[,9])),1)
    u.d.mean.mph <- round(mean(as.numeric(df1[,12])),1)
    legend("topright",
           legend = c(
             "A = arrival",
             "D = departure"
           ),
           cex = c(1,1),
           col = c("red","orange"),
           pch = c(16,16), bty = "n"
    )
    legend("bottomright",
           title = "",
           legend = c(
             expression("Predictions:"),
             bquote(bar(u)[A] == .(u.a.mean.mph)),
             bquote(bar(u)[D] == .(u.d.mean.mph)),
             bquote(bar(w) == .(w.mn)),
             bquote(t[server] == .(tservice))
           ),
           cex = c(0.75,0.75,0.75,0.75)
    )
  }
  print(df1)
  browser()
  return(df1df2)
}
##############################################################################################




