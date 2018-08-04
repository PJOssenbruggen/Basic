#' \code{merge} produces \code{t-x} trajectories for a pair of lead and following vehicles.
#'
#' @return The \code{merge}, a wrapper function for \code{bmfree2}, \code{xabparam} and
#' \code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory.
#' for the following vehicle. The lead vehicle trajectory is not affected.
#' Incidentally, inputs \code{df1} and \code{df2} were created with  \code{bmfree2} and used
#' for testing.
#' @param df1 leading vehicle, a matrix
#' @param df2 following vehicle, a matrix
#' @param leff vehicle length, a number
#' @param step time-step size, a number
#' @param ylim for plot, a vector
#' @usage merge(df1,df2,leff,step,ylim)
# #' @examples
# #' merge(df1,df2,leff,step,ylim)
#' @export
merge <- function(df1, df2,leff,step,ylim) {
  plot(df1[,1],df1[,3], typ  = "l", ylim = ylim,
       xlab = "t, seconds", ylab = expression(x[t]*", feet"))
  lines(df2[,1],df2[,3], lty = 2, col = "orange", lwd = 3)
  abline(v = 0, col = gray(0.8))
  abline(h = 0, col = gray(0.8))
  k   <- 0
  df1 <- df1[,c(1,2,3)]
  df2 <- df2[,c(1,2,3)]
  for(i in 1:5) {
    lst <- xabmerge(df1,df2,leff,step,k, TRUE)
    df3 <- lst[[1]]
    k   <- lst[[2]]
    if(k == 0) {
      df2.fix <- lst[[1]]
      tseq    <- df2.fix[,1]
      useq    <- df2.fix[,4]
      xseq    <- df2.fix[,5]
      df2.fix <- data.frame(t = tseq, u = useq, x = xseq)
      break
    } else {
      dfab   <- lst[[3]]
      print(k)
      print(dfab)
      tstart <- as.numeric(dfab[1])
      tend   <- as.numeric(dfab[2])
      xstart <- as.numeric(dfab[5])
      xend   <- as.numeric(dfab[6])
      points(tstart,xstart)
      points(tend,xend)
      df2    <- df3[,c(1,4,5)]
      tseq   <- seq(tstart, tend, step)
      useq   <- df3[tstart <= df3[,1] & df3[,1] <= tend,4]
      xseq   <- df3[tstart <= df3[,1] & df3[,1] <= tend,5]
      lines(tseq, xseq, lwd = 2)
      df2.fix <- lst[[1]]
      tseq <- df2.fix[,1]
      xseq <- df2.fix[,5]
      lines(tseq, xseq, lwd = 2)
    }
  }
  return(df2.fix)
}
