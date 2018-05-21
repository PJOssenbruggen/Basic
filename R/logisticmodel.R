#' The function \code{logisticmodel} estimates the parameters of logistic regression model
#' using a binomial formula and logit link finction for  \code{quk}
#'
#' @usage logisticmodel()
logisticmodel <- function() {
  qukbin <- quk[,c(1,2,3)]
  bin <- {}
  for(i in 1:dim(qukbin)[1]) {
    if(quk[i,3] < 7.5) bin <- c(bin, 1)                      # k = 5
    if(quk[i,3] >= 7.5 & quk[i,3] < 12.5) bin <- c(bin, 2)   # k = 10
    if(quk[i,3] >= 12.5 & quk[i,3] < 17.5) bin <- c(bin, 3)  # k = 15
    if(quk[i,3] >= 17.5 & quk[i,3] < 22.5) bin <- c(bin, 4)  # k = 20
    if(quk[i,3] >= 22.5 & quk[i,3] < 27.5) bin <- c(bin, 5)  # k = 25
    if(quk[i,3] >= 27.5 & quk[i,3] < 32.5) bin <- c(bin, 6)  # k = 30
    if(quk[i,3] >= 32.5 & quk[i,3] < 37.5) bin <- c(bin, 7)  # k = 35
    if(quk[i,3] >= 37.5 & quk[i,3] < 42.5) bin <- c(bin, 8)  # k = 40
    if(quk[i,3] >= 42.5 & quk[i,3] < 47.5) bin <- c(bin, 9)  # k = 45
    if(quk[i,3] >= 47.5 & quk[i,3] < 52.5) bin <- c(bin, 10)  # k = 50
    if(quk[i,3] >= 52.5 & quk[i,3] < 57.5) bin <- c(bin, 11)  # k = 55
    if(quk[i,3] >= 57.5 & quk[i,3] < 62.5) bin <- c(bin, 12)  # k = 60
    if(quk[i,3] >= 62.5 & quk[i,3] < 67.5) bin <- c(bin, 13)  # k = 65
    if(quk[i,3] >= 67.5 & quk[i,3] < 72.5) bin <- c(bin, 14)  # k = 70
    if(quk[i,3] >= 72.5 & quk[i,3] < 77.5) bin <- c(bin, 15)  # k = 75
    if(quk[i,3] >= 77.5 & quk[i,3] < 82.5) bin <- c(bin, 16)  # k = 80
    if(quk[i,3] >= 82.5 & quk[i,3] < 87.5) bin <- c(bin, 17)  # k = 85
    if(quk[i,3] >= 87.5 & quk[i,3] < 92.5) bin <- c(bin, 18)  # k = 90
    if(quk[i,3] >= 92.5 & quk[i,3] < 97.5) bin <- c(bin, 18)  # k = 95
    if(quk[i,3] >= 97.5 & quk[i,3] < 102.5) bin <- c(bin, 19)  # k = 100
    if(quk[i,3] >= 102.5 & quk[i,3] < 107.5) bin <- c(bin, 20)  # k = 105
    if(quk[i,3] >= 107.5 & quk[i,3] < 112.5) bin <- c(bin, 21)  # k = 110
    if(quk[i,3] >= 112.5 & quk[i,3] < 117.5) bin <- c(bin, 22)  # k = 115
    if(quk[i,3] >= 117.5 & quk[i,3] < 122.5) bin <- c(bin, 23)  # k = 120
    if(quk[i,3] >= 122.5) bin <- c(bin, 24)                     # k = 125
  }
  qukbin <-  cbind(qukbin, bin)
  pi.forecast <- function(k) exp(fit1$coefficients[1] + fit1$coefficients[2] * k)/(1 + exp(fit1$coefficients[1] + fit1$coefficients[2] * k))
  pf = y <- n <- k <- rep(NA, 23)
  df <- data.frame(k, y, n, pf)
  kseq <- seq(5,125,5)
  for(i in 1:24) {
    ui      <- qukbin[qukbin[,4] == i,2]
    df[i,1] <- kseq[i]
    df[i,2] <- m <- length(ui[ui <= 50])
    df[i,3] <- n <- length(ui[ui > 50])
    df[i,4] <- m/(m + n)
  }
  plot(df[,1], df[,4], typ = "p", pch = 16, xlab = "k, veh/mi", ylab = expression(pi) )
  k <- df[,1]
  df[,1] <- k
  fit1 <- glm2(pf ~ k, data = df, family = binomial(link="logit"),
               control=glm.control(trace=TRUE))
  k.factor <- gl(n = dim(df)[1], k = 1, labels = as.character(k))
  df <- cbind(df, k.factor)
 # fit2 <- glm2(pf ~ k.factor, data = df, family = binomial(link="logit"),
 #              control=glm.control(trace=TRUE))
  kseq <- seq(0,120,0.1)
  pi.hat <- pi.forecast(kseq)
  pi.hat2 = pi.forecast(df[,1])
  lines(kseq, pi.hat)
  exp.failures <- pi.hat2 * (df[,2] + df[,3])
  df <- cbind(df, pi.hat2, exp.failures)
  points(df[,1], df[,6])
  e <- df[,2] - exp.failures
  df <- cbind(df, e)
  return(df)
}
