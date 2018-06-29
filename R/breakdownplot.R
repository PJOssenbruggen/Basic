#' The function \code{breakdownplot} plots the data from the \code{accelpass} data frame.
#'
#' @param df from \code{accelpass}, a matrix
#' @param title, a character string
#' @usage breakdownplot(df, title)
# #' examples
# #' breakdownplot(df, title = "Free-Flow Pass and Merge")
#' @export
breakdownplot <- function(df, title) {
  par(mfrow = c(1,3))
  plot(df[,7], df[,3], typ = "l",  xlim = c(-7,7), col = "green", axes = FALSE, ylab = "", xlab = "", cex = 2)
  lines(df[,7], df[,6])
  text(3,0, expression("x = 0"))
  text(0,-500, expression("x = -500"))
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, -500), col = gray(0.8))
  plot(df[,1], df[,5], typ = "l", xlab = "t, seconds", ylab = expression(u[t]*", fps"))
  plot(df[,1], df[,6], typ = "l", xlab = "t, seconds", ylab = expression(x[t]*", feet"))
  lines(df[,1], df[,3], col = gray(0.8))
  title(main = title)

  df1 <- accelpass(0, 41, 11, -1000, -500, 1000, 14, 1, 0.25)
  df2 <- accelpass(0, 41, 11, -1000, -500, 1000, 14, 2, 0.25)
  par(mfrow = c(1,1))
  plot(df1[,1], df1[,2], typ = "l", xlab = "t, seconds", ylab = expression(u[t]*", fps"),
       col = gray(0.5), ylim = c(0,120))
  lines(df1[,1], df1[,5], col = "red")
  lines(df2[,1], df2[,5], col = "blue")

  par(mfrow = c(1,1), pty = "s")
  plot(df1[,1], df1[,3], typ = "l", xlab = "t, seconds", ylab = expression(x[t]*", feet"),
       col = gray(0.5), lwd = 1)
  lines(df1[,1], df1[,6], col = "red")
  lines(df2[,1], df2[,6], col = "blue")


  par(mfrow = c(1,1))
  plot(df1[,7],df1[,3], typ = "l",  xlim = c(-7,7), ylim = c(-500,0),
       col = gray(0.5), axes = FALSE, ylab = "", xlab = "")
  lines(df1[,7], df1[,6], col = "red")
  lines(df2[,7], df2[,6], col = "blue")
  text(3,0, expression("x = 0"))
  text(0,-500, expression("x = -500"))
  abline(v = 0, col = gray(0.8))
  abline(h = c(0, -500), col = gray(0.8))
}
