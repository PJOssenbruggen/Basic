#' \code{cfanim} uses a Brownian motion model of speed to describe driver's ability to maintain a speed \code{u}.
#'
#' @return The \code{cfanim} function returns information for a single vehicle
#' used to analyze the effects of traffic noise on car-following.
#' The function uses a stochastic traffic noise model, a brownian motion or Weiner \code{W} model.
#' @param u mean speed (mph), a number
#' @param usd standard deviation of \code{umn}, a number
#' @param tup upper time range, a number
#' @param dt time-step, a number
#' @usage cfanim(u, usd, tup, dt)
#' @examples
#' cfanim(60, 3, 30, 0.25)
cfanim <- function(u, usd, tup, dt) {
  u. <- u
  n <- tup/dt
  xlim = c(0, tup)
  ylim = c(0,u + 10*usd)
  plot(x = 0, y = u, xlim = xlim, ylim = ylim, pch = 16, typ = "p",
       xlab = "t, seconds", ylab = expression(u[t]*", mph"))
  abline(v = 0, col = gray(0.8))
  abline(h = u, col = gray(0.8))
  Wold <- 0
  i <- 0
  df <- data.frame(t = dt*i, u = u.)
  for (i in 1:n) {i
    dev.hold()
    t <- dt * i
    W <- Wold + sqrt(dt) *  rnorm(1)
    u <- u. + usd * W
    points(t, u, pch = 16)
    Wold <- W
    df <- rbind(df, data.frame(t = dt*i,  u = u))
  }
  p    <- ggplot2::ggplot(df, ggplot2::aes(t, u)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::ggtitle("Brownian Motion Model of Speed")
  return(p)
}
