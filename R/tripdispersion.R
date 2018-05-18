#' Estimate the parameters \code{a} and \code{b} of a second-order velocity model.
#'
#' @param tend time, a number
#' @param  utrav  number of travelers, a number
#' @usage  tripdispersion(tend, ntrav, "Trip Dispersion")
#' @examples
#' tripdispersion(20, 123)
tripdispersion = function(tend, seed) {
  set.seed(seed)
  tseq <- seq(1,tend)
  x <- y <- length(tseq)
  x[1] <- y[1] <- 0
  for(k in 2:length(tseq)) {
      x[k] <- x[k-1] + rnorm(1)
      y[k] <- y[k-1] + rnorm(1)
  }
  x <- x[!is.na(x)]
  y <- y[!is.na(x)]
  df <- data.frame(t = tseq, x, y)
  p   <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "glm")
  return(p)
}
