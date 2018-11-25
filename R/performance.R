#' \code{performance} looks at waiting time and performance measures.
#'
#' @return The \code{performance} returns scatter plots, histograms and statistical summaries for two data sets.
#' @param run1, a matrix
#' @param run2, a matrix
#' @param p1 proportion free flow, a number
#' @param p2 proportion free flow, a number
#' @usage performance(run1,run2,p1,p2)
#' @export
performance <- function(run1,run2,p1,p2) {
  p.sd1  <- sqrt(p1*(1-p1))
  p.sd2  <- sqrt(p2*(1-p2))
  colnames(run1) <- c("mean1", "SD1")
  colnames(run2) <- c("mean2", "SD2")
  n1 <- n2 <- 100
  se1 <- se2 <- se <- z <- P <- rep(NA,8)
  for(i in 1:7) {
    se1[i]  <- run1[i,2] / sqrt(n1)
    se2[i]  <- run2[i,2] / sqrt(n2)
  }
  for(i in 1:7) se[i] <- sqrt(se1[i]^2 + se2[i]^2)
  run1 <- cbind(run1,se1)
  run2 <- cbind(run2,se2)
  run  <- cbind(run1,run2,diff = run1[,1] - run2[,1], se, z, P)
  for(i in 1:7) run[i,9]  <- run[i,7]/run[i,8]
  for(i in 1:7) run[i,10] <- pnorm(run[i,9])
  names(run) <- c("zipper.mean","zipper.SD","zipper.se","sbs.mean","sbs.SD","sb.se","diff","se","z","P")
  diff  <- p1-p2
  p.se1 <- p.sd1/sqrt(n1)
  p.se2 <- p.sd2/sqrt(n2)
  p.se  <- sqrt(p.se1^2 + p.se2^2)
  z     <- diff/p.se
  p     <- pnorm(z)
  p.free <- as.matrix(data.frame(p1,p.sd1,p.se1,
                                 p2,p.sd2,p.se2,
                                 p.se, diff, z, p))
  colnames(p.free) <- c("zipper.mean","zipper.SD","zipper.se","sbs.mean","sbs.SD","sb.se","diff","se","z","P")
  row.names(p.free) <- "proportion.free"
  print(p.free)
  run   <- rbind(run, p.free)
  run   <-round(run,2)
  return(run)
}
