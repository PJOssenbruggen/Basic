#' \code{cfanim} uses a stochastic model of speed to describe driver's ability to maintain a speed \code{u}.
#'
#' @return The \code{cfanim} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The function uses a stochastic traffic noise model, a Brownian motion or Wiener \code{W} model.
#' @param umn mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param tup upper time range, a number
#' @param dt time-step, a number
#' @usage cfanim(umn, usd, tup, dt)
# #' @examples
# #' cfanim(60, 3, 30, 0.25)
cfanim <- function(umn, usd, tup, dt) {
  N <- tup/dt
  rn <- rnorm(N)
  W <- c(0, rn)
  tseq <- seq(0,tup,dt)
  u <- umn + usd * W
  df <- data.frame(t = tseq, u = u, W, rn = c(0, rn))
  names(df) <- c("t", "u", "W", "rn")
  return(df)
}
