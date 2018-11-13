#' \code{chancemodel} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{chancemodel} calculates the mean3 and standard deviations from \code{simulate}.
#' @param output \code{simulate} output, a data frame
#' @param outputruns is a collection of \code{output} runs, a data frame.
#' @usage chancemodel(outputruns, output)
#' @export
chancemodel <- function(outputruns, output) {
  print(output)
  outputruns <- rbind(outputruns, output)
  sum.out1   <- data.frame(colMeans(outputruns,na.rm=TRUE))
  sum.out2   <- data.frame(apply(outputruns,2,sd,na.rm=TRUE))
  sum.out    <- cbind(sum.out1,sum.out2)
  colnames(sum.out) <- c("mean","SD")
  print(sum.out)
  return(list(outputruns,sum.out))
}
