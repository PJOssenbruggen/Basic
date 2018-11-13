#' \code{performance} looks at waiting time and performance measures.
#'
#' @return The \code{performance} returns scatter plots, histograms and statistical summaries for two data sets.
#' @param run1, a data frame
#' @param run2, a data frame
#' @usage performance(run1,run2)
#' @export
performance <- function(run1,run2) {
  mn1        <- colMeans(run1,na.rm=TRUE)
  sd1        <- apply(run1,2,sd,na.rm=TRUE)
  out1       <- cbind(mn1,sd1)
  colnames(out1) <- c("mean.sbs","SD.sbs")
  mn2        <- colMeans(run2,na.rm=TRUE)
  sd2        <- apply(run2,2,sd,na.rm=TRUE)
  out2       <- cbind(mn2,sd2)
  colnames(out2) <- c("mean.zipper","SD.zipper")
  z          <- p.value   <- rep(0,8)
  output     <- cbind(out1,out2,z,p.value)

  n1         <- dim(run1)[1]
  n2         <- dim(run2)[1]
  for(i in 1:7) {
    se1           <- output[i,2] * sqrt(n1)/n1
    se2           <- output[i,4] * sqrt(n2)/n2
    se            <- sqrt(se1^2 + se2^2)
    output[i,5]   <- (output[i,1] - output[i,3])/se
  }
  output      <- round(output,1)
  for(i in 1:7)  output[i,6] <- round(pnorm(abs(output[i,5])),3)
  print(output)
  par(mfrow = c(2,2), pty = "s")
  plot(run1[,4], run1[,6], pch = 16, col = "green",
       xlab = expression(k[d]), ylab = expression(q[d]), xlim = c(0,200), ylim = c(0, 3000))
  title(main="Side-by-Side Merge")
  plot(run2[,4], run2[,6], pch = 16, col = "green",
       xlab = expression(k[d]), ylab = expression(q[d]), xlim = c(0,200), ylim = c(0, 3000))
  title(main = "Zipper Merge")
  hist(run1[,7], col = "wheat", xlim = 1.2*c(0, max(c(run1[,7],run2[,7]))),
       main = "Side-by-Side Merge", xlab = expression(bar(w)))
  box()
  hist(run2[,7], col = "wheat", xlim = 1.2*c(0,max(c(run1[,7],run2[,7]))),
       main = "Zipper Merge", xlab = expression(bar(w)))
  box()
}
