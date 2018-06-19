#' The \code{usdBBridge} function from Iacus.
#'
#' @return The \code{usdBBridge} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The model, a stochastic traffic noise model, is assumed to be a Brownian bridge model.
#' Distance \code{x} is estimated with the forward Euler method.
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param T upper time range in minutes, a number
#' @param N number of time-steps, a number
#' @usage usdBBridge(umn, usd, N, T)
#' @examples
#' usdBBridge(41, 11, 90000, 15)
#' usdBBridge(18.8, 3.9, 90000, 15)
usdBBridge <- function(umn, usd, N, T) {
  W     <- numeric(N+1)
  t     <- seq(0, T, length = N+1)
  dt    <- T/N
  for(i in 2:(N+1)) W[i] <- W[i-1] + sqrt(dt) * usd * rnorm(1)
  x     <- umn
  y     <- umn
  u     <- x + (W - t/T *  (W[N+1] - y + x))
  plot(t, u, type = "l", xlab = "t, minutes", ylab = expression(u[t]*", mph"),
       ylim = c(0, umn + 3 * usd), lwd = 2)
  abline(h = umn, lty = 3)
  abline(h = c(umn + 1.96 * usd, umn - 1.96 * usd), lty = 2, lwd = 1)
  abline(h = 0, col = gray(0.3))
  abline(v = 0, col = gray(0.3))
  n1   <- 120
  df0  <- data.frame(mean = umn, sd = usd, N = 0, dt)
  df2  <- data.frame(mean = mean(u), sd = sd(u), N, dt)
  df1  <- data.frame(mean = mean(u[1:n1]), sd = sd(u[1:n1]), N = n1, dt)
  df   <- round(rbind(df0, df2, df1), 2)
  legend("topright",
         legend = c(expression(bar(u)[t]), "95% C.I."),
         lty = c(1,2),
         lwd = c(2,1),
         bty = "n")
  return(df)
}
