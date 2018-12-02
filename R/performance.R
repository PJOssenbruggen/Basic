#' \code{performance} looks at waiting time and performance measures.
#'
#' @return The \code{performance} returns statistical summaries for two data sets.
#' @param sbs.summary, a matrix
#' @param zipper.summary, a matrix
#' @param sbs.prop.free proportion free flow, a number
#' @param zipper.prop.free proportion free flow, a number
#' @param size sample zize, a number
#' @usage performance(sbs.summary,zipper.summary,sbs.prop.free,zipper.prop.free,size)
#' @export
performance <- function(sbs.summary,zipper.summary,sbs.prop.free,zipper.prop.free,size) {
  p.sd1  <- sqrt(sbs.prop.free*(1-sbs.prop.free))
  p.sd2  <- sqrt(zipper.prop.free*(1-zipper.prop.free))
  colnames(sbs.summary) <- c("mean1", "SD1")
  colnames(zipper.summary) <- c("mean2", "SD2")
  n1 <- n2 <- size
  se1 <- se2 <- se <- z <- P <- rep(NA,8)
  for(i in 1:7) {
    se1[i]  <- sbs.summary[i,2] / sqrt(n1)
    se2[i]  <- zipper.summary[i,2] / sqrt(n2)
  }
  for(i in 1:7) se[i] <- sqrt(se1[i]^2 + se2[i]^2)
  sbs.summary <- cbind(sbs.summary,se1)
  zipper.summary <- cbind(zipper.summary,se2)
  run  <- cbind(sbs.summary,zipper.summary,diff = sbs.summary[,1] - zipper.summary[,1], se, z, P)
  for(i in 1:7) run[i,9]  <- run[i,7]/run[i,8]
  for(i in 1:7) run[i,10] <- pnorm(run[i,9])
  names(run) <- c("zipper.mean","zipper.SD","zipper.se","sbs.mean","sbs.SD","sb.se","diff","se","z","P")
  diff  <- sbs.prop.free-zipper.prop.free
  p.se1 <- p.sd1/sqrt(n1)
  p.se2 <- p.sd2/sqrt(n2)
  p.se  <- sqrt(p.se1^2 + p.se2^2)
  z     <- diff/p.se
  p     <- pnorm(z)
  p.free <- as.matrix(data.frame(sbs.prop.free,p.sd1,p.se1,
                                 zipper.prop.free,p.sd2,p.se2,
                                 p.se, diff, z, p))
  colnames(p.free) <- c("zipper.mean","zipper.SD","zipper.se","sbs.mean","sbs.SD","sb.se","diff","se","z","P")
  row.names(p.free) <- "proportion.free"
  print(p.free)
  run   <- rbind(run, p.free)
  run   <- round(run,2)
  return(run)
}
