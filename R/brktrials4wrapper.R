#' \code{brktrials4wrapper} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
#'
#' @return The \code{brktrials4wrapper}, a wrapper function for \code{bmfree3}, \code{xabparam} and
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
#' @param run, a number
#' @param kfactor density at time \code{t} = 0, a number
#' @param browse to inspect \code{fixviolation} to inspect plot or FALSE otherwise
#' @usage brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,run,kfactor,browse)
# #' @examples
# #' brktrials4wrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,run,kfactor,browse)
#' @export
brktrials4wrapper  <- function(nveh1,nveh2,umn,usd,xstart1,xstart2,step,tstart,tend,xfunnel,leff,run,kfactor,browse) {
#### STEP 1. Create lane 1 and 2 datasets ################################################################
  set.seed(123)
  # set.seed(403)
  #set.seed(333)
  xlim  <- c(tstart,tend)
#  print(data.frame(nveh1,nveh2,xstart1,xstart2,step,tstart,tend,xfunnel,leff))
  zippermerge(nveh, tstart, tend, xstart, umn, leff, xfunnel, step, TRUE, kfactor)
  lst   <- brktrials3wrapper(nveh1,nveh2,umn,usd,tstart,tend,xstart1,xstart2,step,run,leff,xfunnel,kfactor,browse=FALSE)
  lane1 <- lst[[1]]
  lane2 <- lst[[2]]
  if(run == 1) {
    pdf(file = "/Users/PJO/Desktop/Lanes1_2.pdf")
    par(mfrow = c(1,2), pty = "s")
    nclm    <- seq(2, nveh1*3, 3)
    min.    <- min(as.numeric(unlist(lane1[,nclm])), na.rm = TRUE)
    max.    <- max(as.numeric(unlist(lane1[,nclm])), na.rm = TRUE)
    ylim    <- c(min., max.)
    # Lane 1
    tseq   <- seq(0, tend, step)
    tlen   <- length(tseq)
    tend   <- tseq[tlen]
    tend.0 <- tend
    plot(tseq, lane1[,2], type = "l", xlab = "t, seconds", ylab = "x, feet",
         ylim=ylim, xlim=xlim, col = "blue", lwd = 2)
    df       <- flow(lane1, tstart, tend, step, xfunnel, FALSE)[[2]]
    speeda   <- as.numeric(df[1])
    speedd   <- as.numeric(df[2])
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane1[,2]), labels = 1, pos = 4, offset = 0.2, cex = 0.5)
    text(0, min(lane1[,2]), labels = 1, pos = 2, offset = 0.2, cex = 0.5)
    nveh1  <- dim(lane1)[2]/3
    if(usd == 0) sub <- "Lane Analyzer" else sub <- "Lane Analyzer"
    axis(side = 3, at = tend/2, sub, tick = FALSE, line = -1)
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    for(veh in 2:nveh1) {
      xcol <- 3*(veh-1) + 2
      lines(tseq,lane1[,xcol], col = "blue", lwd = 2)
      text(tend, max(lane1[,xcol]), labels = veh,pos = 4, offset = 0.2, cex = 0.5)
      text(0, min(lane1[,xcol]), labels = veh,pos = 2, offset = 0.2, cex = 0.5)
    }
    title(main = "Lane 1", sub = "Car-following effects.")
    density <- as.numeric(5280/hsafe(umn*5280/3600,leff))/kfactor
    density <- round(density,0)
    legend("topleft",
    #  title = "",
      legend = c(
        expression(""),
        bquote(u[0] == .(umn)),
        bquote(sigma[U] == .(usd)),
        bquote(k[0] == .(density)),
        bquote(bar(u)[A] == .(speeda)),
        bquote(bar(u)[D] == .(speedd))
     ),
      cex = c(0.75,0.75,0.75))
    # Lane 2
    nclm    <- seq(2, nveh2*3, 3)
    min.    <- min(as.numeric(unlist(lane2[,nclm])), na.rm = TRUE)
    max.    <- max(as.numeric(unlist(lane2[,nclm])), na.rm = TRUE)
    ylim    <- c(min., max.)
    plot(tseq, lane2[,2], type = "l", xlab = "t, seconds", ylab = "x, feet",
         ylim=ylim, xlim=xlim,col = "red", lwd = 2)
    df      <- flow(lane2, tstart, tend, step, xfunnel, FALSE)[[2]]
    speeda   <- as.numeric(df[1])
    speedd   <- as.numeric(df[2])
    density <- as.numeric(5280/hsafe(umn*5280/3600,leff))/kfactor
    density <- round(density,0)
    if(usd == 0) sub <- "Lane Analyzer" else sub <- "Lane Analyzer"
    axis(side = 3, at = tend/2, sub, tick = FALSE, line = -1)
    abline(v = 0, col = gray(0.8))
    abline(h = c(0, xfunnel), col = gray(0.8))
    text(tend, max(lane2[,2]), labels = 1,pos = 4, offset = 0.2, cex = 0.5)
    text(0, min(lane2[,2]), labels = 1,pos = 2, offset = 0.2, cex = 0.5)
    nveh2  <- dim(lane2)[2]/3
    for(veh in 2:nveh2) {
      xcol <- 3*(veh-1) + 2
      lines(tseq,lane2[,xcol], col = "red", lwd = 2)
      text(tend, max(lane2[,xcol]), labels = veh, pos =4, offset = 0.2, cex = 0.5)
      text(0, min(lane2[,xcol]), labels = veh,pos = 2, offset = 0.2, cex = 0.5)
    }
    title(main = "Lane 2", sub = "Vehicle grouping.")
    axis(side = 4, at = 0, labels = expression(x[0]))
    axis(side = 4, at = xfunnel, labels = expression(x[e]))
    legend("topleft",
      #     title = "",
           legend = c(
             expression(""),
             bquote(u[0] == .(umn)),
             bquote(sigma[U] == .(usd)),
             bquote(k[0] == .(density)),
             bquote(bar(u)[A] == .(speeda)),
             bquote(bar(u)[D] == .(speedd))
           ),
           cex = c(0.75,0.75,0.75))
    dev.off()
  }
#  if(browse == TRUE) browser()
#### STEP 2. Order vehicles by the times that they reach x = 0 ####################
  lst    <- enterbottleneck(lane1,lane2,xfunnel,tend,step)
  dfcrit <- lst[[1]]
  nveh   <- lst[[2]]
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
#  print(data.frame("brktrials4wrapper / dfcrit"))
#  print(dfcrit)
### Zipper Merge ################################################################
  if(usd != 0) {
    print(data.frame("brktrials4wrapper / dfcrit"))
#    print(dfcrit)
    # Fix violation for Lane Drop #################################################
    zone <- 2
    df1df2.fix    <- df1df2
    for(veh in 2:nveh) {
      df1df2.fix  <- fixviolation(veh, zone, df1df2, dfcrit, step, tend.0, leff, xfunnel, FALSE, browse = FALSE)
      df1df2.fix  <- fixdf1df2(veh, df1df2.fix, df1df2)
      df1df2      <- df1df2.fix
    }
    # Bottleneck plot
    xstart <- xstart1
    df1    <- flow2(dfcrit, df1df2, tstart, tend, step, xfunnel, FALSE)
    if(run == 1) {
      pdf(file = "/Users/PJO/Desktop/Merge.pdf")
      par(mfrow = c(1,1), pty = "s")
      tend <- tseq[tlen]
      xcol <- {}
      for(veh in 1:nveh) xcol <- c(xcol, 2 + 3 * (veh-1))
      xval <- df1df2[,xcol]
      xlim <- c(0, tend + 3)
      ylim <- c(min(xval), max(xval))
      plot(tseq, df1df2[,2], type = "n", xlab = "t, seconds",ylab = "x, feet",
           ylim=ylim, xlim=xlim)
      if(xstart1 == xstart2) subtitle <- "Side-by-side merge." else subtitle = "Zipper merge."
      title(main = "Bottleneck", sub = subtitle)
      abline(h = c(0,xfunnel), col = gray(0.8))
      abline(v = c(0), col = gray(0.8))
      sub <- "Merge Analyzer"
      axis(side = 3, at = tend/2, sub, tick = FALSE, line = -1)
      axis(side = 4, at = 0, labels = expression(x[0]))
      axis(side = 4, at = xfunnel, labels = expression(x[e]))
      u.a.mean.mph <- round(mean(as.numeric(df1[,9])),1)
      u.d.mean.mph <- round(mean(as.numeric(df1[,12])),1)
      legend("topleft",
      #       title = "",
             legend = c(
               expression(""),
               bquote(u[0] == .(umn)),
               bquote(sigma[U] == .(usd)),
               bquote(k[0] == .(density)),
               bquote(bar(u)[A] == .(u.a.mean.mph)),
               bquote(bar(u)[D] == .(u.d.mean.mph))
             ),
             cex = c(0.75,0.75,0.75)
      )
      for(veh in 1:nveh) {
        xcol     <- 2 + 3 * (veh-1)
        if(dfcrit[veh,1] == 1) lines(tseq,df1df2[,xcol], lty = 1, col = "blue", lwd = 2) else {
          lines(tseq,df1df2[,xcol], lty = 1, col = "red", lwd = 2)
        }
        ln   <- as.numeric(dfcrit[veh,1])
        vh   <- as.numeric(dfcrit[veh,2])
        lab1 <- paste("(",sep="",ln)
        lab2 <- paste(lab1,sep="",",")
        lab3 <- paste(lab2,sep="",vh)
        lab  <- paste(lab3,sep="",")")
        text(tend, max(as.numeric(df1df2[,xcol])), labels = lab, pos = 4, offset = 0.2, cex = 0.75)
      }
      dev.off()
    }
#    if(browse == TRUE) browser()
#### STEP 6. Flow Estimation ######################################################
    lst <- flow(df1df2, tstart, tend, step, xfunnel,TRUE)
    df1 <- lst[[1]]
    df2 <- lst[[2]]
#### STEP 7. Capacity Estimation
    kq    <- flow2(dfcrit, df1df2, tstart, tend, step, xfunnel,TRUE)
    tservice = abs(round(xfunnel/(5280/3600*umn),1))
    k.d.mn <- round(mean(kq[,7], na.rm = TRUE),0)
    q.d.mn <- round(mean(kq[,8], na.rm = TRUE),0)
    k.a.mn <- round(mean(kq[,10], na.rm = TRUE),0)
    q.a.mn <- round(mean(kq[,11], na.rm = TRUE),0)
    w.mn   <- round(mean(kq[,6], na.rm = TRUE),1)
    u.a.mean.mph <- round(mean(as.numeric(kq[,9])),1)
    u.d.mean.mph <- round(mean(as.numeric(kq[,12])),1)
      if(run == 1) {
        pdf(file = "/Users/PJO/Desktop/QueueAnalyzer.pdf")
        par(mfrow = c(1,2), pty = "s")
        plot(df1[,2], df1[,1], typ = "s", xlim = c(0,max(df1[,3])), ylim = c(1,nveh+1.25),
             ylab = "Vehicle", xlab = "t, seconds", col = "red", lwd = 2)
        lines(df1[,3], df1[,1], typ = "s", col = "orange", lwd = 2)
        abline(v = 0, col = gray(0.8))
        abline(h = 1, col = gray(0.8))
        if(xstart1 == xstart2) subtitle <- "Side-by-side merge." else subtitle = "Zipper merge."
        title(main = "Bottleneck", sub = subtitle)
        sub <- "Queue Analyzer"
        axis(side = 3, at = max(df1[,3])/2, sub, tick = FALSE, line = -1)
        legend("topleft", legend = c("A = arrival","D = departure"), lty = c(1,1), col = c("red","orange"), bty = "n")
        u.a.mean.mph <- as.numeric(df2[1])
        u.d.mean.mph <- as.numeric(df2[2])
        #      dev.off()
      xlim <- c(0, 200)
      ylim <- c(0, 4000)
      plot(kq[,10], kq[,11], typ = "p", xlim, ylim, ylab = "q, vph",
           xlab = "k, vpm", pch = 16, col = "red")
      points(kq[,7],kq[,8],pch = 16, col = "orange")
      sub = expression("Service time: "*W[t] == D(t) - A(t))
      title(main = "Bottleneck" , sub)
      k.d.mn <- round(mean(kq[,7], na.rm = TRUE),0)
      q.d.mn <- round(mean(kq[,8], na.rm = TRUE),0)
      k.a.mn <- round(mean(kq[,10], na.rm = TRUE),0)
      q.a.mn <- round(mean(kq[,11], na.rm = TRUE),0)
      w.mn   <- round(mean(kq[,6], na.rm = TRUE),1)
      axis(side = 3, at = k.a.mn, labels = expression(bar(k)[A]), tick = FALSE, line = -1)
      axis(side = 3, at = k.d.mn, labels = expression(bar(k)[D]), tick = FALSE, line = -1)
      abline(h = 0, col = gray(0.8))
      abline(v = 0, col = gray(0.8))
      abline(h = q.a.mn, lty = 2, col = "red")
      abline(h = q.d.mn, lty = 2, col = "orange")
      abline(v = k.a.mn, lty = 2,col= "red")
      abline(v = k.d.mn, lty = 2,col= "orange")
      axis(side = 4, at = q.d.mn, labels = expression(bar(q)[D]), tick = FALSE, line = -0.75)
      axis(side = 4, at = q.a.mn, labels = expression(bar(q)[A]), tick = FALSE, line = -0.75)
      u.a.mean.mph <- round(mean(as.numeric(kq[,9])),1)
      u.d.mean.mph <- round(mean(as.numeric(kq[,12])),1)
      legend("topright", legend = c("A = arrival","D = departure"),
             pch = c(16,16), col = c("red","orange"), bty = "n")
      legend("bottomright",
       #      title = "",
             legend = c(
               expression(""),
               bquote(u[0] == .(umn)),
               bquote(sigma[U] == .(usd)),
               bquote(k[0] == .(density)),
               bquote(bar(u)[A] == .(u.a.mean.mph)),
               bquote(bar(u)[D] == .(u.d.mean.mph)),
               bquote(bar(w) == .(w.mn)),
               bquote(t[server] == .(tservice))
             ),
             cex = c(0.75,0.75,0.75,0.75)
             )
      dev.off()
      }
    if(is.infinite(q.a.mn)) q.a.mn <- NA
    if(is.infinite(q.d.mn)) q.d.mn <- NA
    run.df <- data.frame(
      u.a = u.a.mean.mph, u.d = u.d.mean.mph,
      k.a = k.a.mn, k.d = k.d.mn,
      q.a = q.a.mn, q.d =  q.d.mn,
      w = w.mn, tservice)
    return(run.df)
  }
  if(browse == TRUE) browser()
}
##############################################################################################

