#' The function \code{speedboxplot} plots the data from a data frame \code{QUKtdayX}
#'
#' @param QUKtdayX, a matrix
#' @usage speedboxplot(QUKtdayX)
speedboxplot <- function(QUKtdayX) {
### Density-speed. Put in bins.
  quk1 <- QUKtdayX[,c(1,2,3)]
  bin <- {}
  for(i in 1:dim(quk1)[1]) {
    if(quk1[i,3] < 7.5) bin <- c(bin, 1)                      # k = 5
    if(quk1[i,3] >= 7.5 & quk1[i,3] < 12.5) bin <- c(bin, 2)   # k = 10
    if(quk1[i,3] >= 12.5 & quk1[i,3] < 17.5) bin <- c(bin, 3)  # k = 15
    if(quk1[i,3] >= 17.5 & quk1[i,3] < 22.5) bin <- c(bin, 4)  # k = 20
    if(quk1[i,3] >= 22.5 & quk1[i,3] < 27.5) bin <- c(bin, 5)  # k = 25
    if(quk1[i,3] >= 27.5 & quk1[i,3] < 32.5) bin <- c(bin, 6)  # k = 30
    if(quk1[i,3] >= 32.5 & quk1[i,3] < 37.5) bin <- c(bin, 7)  # k = 35
    if(quk1[i,3] >= 37.5 & quk1[i,3] < 42.5) bin <- c(bin, 8)  # k = 40
    if(quk1[i,3] >= 42.5 & quk1[i,3] < 47.5) bin <- c(bin, 9)  # k = 45
    if(quk1[i,3] >= 47.5 & quk1[i,3] < 52.5) bin <- c(bin, 10)  # k = 50
    if(quk1[i,3] >= 52.5 & quk1[i,3] < 57.5) bin <- c(bin, 11)  # k = 55
    if(quk1[i,3] >= 57.5 & quk1[i,3] < 62.5) bin <- c(bin, 12)  # k = 60
    if(quk1[i,3] >= 62.5 & quk1[i,3] < 67.5) bin <- c(bin, 13)  # k = 65
    if(quk1[i,3] >= 67.5 & quk1[i,3] < 72.5) bin <- c(bin, 14)  # k = 70
    if(quk1[i,3] >= 72.5 & quk1[i,3] < 77.5) bin <- c(bin, 15)  # k = 75
    if(quk1[i,3] >= 77.5 & quk1[i,3] < 82.5) bin <- c(bin, 16)  # k = 80
    if(quk1[i,3] >= 82.5 & quk1[i,3] < 87.5) bin <- c(bin, 17)  # k = 85
    if(quk1[i,3] >= 87.5 & quk1[i,3] < 92.5) bin <- c(bin, 18)  # k = 90
    if(quk1[i,3] >= 92.5 & quk1[i,3] < 97.5) bin <- c(bin, 18)  # k = 95
    if(quk1[i,3] >= 97.5 & quk1[i,3] < 102.5) bin <- c(bin, 19)  # k = 100
    if(quk1[i,3] >= 102.5 & quk1[i,3] < 107.5) bin <- c(bin, 20)  # k = 105
    if(quk1[i,3] >= 107.5 & quk1[i,3] < 112.5) bin <- c(bin, 21)  # k = 110
    if(quk1[i,3] >= 112.5 & quk1[i,3] < 117.5) bin <- c(bin, 22)  # k = 115
    if(quk1[i,3] >= 117.5 & quk1[i,3] < 122.5) bin <- c(bin, 23)  # k = 120
    if(quk1[i,3] >= 122.5) bin <- c(bin, 24)                     # k = 125
  }

  par(mfrow = c(1,1))
  graphics::boxplot(u ~ bin, data = quk1, col = gray(0.8), axes = FALSE)
  abline(h = 45, lty = 3)
  abline(v = 9, lty = 3)
  axis(side = 2, at = seq(0,80, by = 20))
  axis(side = 2, at = 40, labels = "u, mi / h", tick = FALSE, line = 2)
  axis(side = 1, at = seq(2,24, by = 2), labels = seq(10,120,by=10))
  axis(side = 1, at = 11, labels = "k, veh / mi", tick = FALSE, line = 2)
  axis(side = 3, at = 9, labels = expression(k^"*"))
  axis(side = 4, at = 45, labels = expression(u^"*"))
  box()
}
