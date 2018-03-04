#' The function \code{speedplot} plots the data from a data frame \code{df}
#'
#' @usage speedplot()
speedplot <- function() {
  t <- u <- NULL
  df <- dplyr::select(I93speed, t, u)
  ggplot2::ggplot(df, ggplot2::aes(t, u)) +
    ggplot2::geom_point() + ggplot2::geom_smooth(span = 0.3, se = FALSE) +
    ggplot2::ggtitle("Breakdown at the Bottleneck")
}
