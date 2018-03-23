#' The function \code{speedplot} plots the data from the \code{quk}  data frame.
#'
#' @usage speedplot()
speedplot <- function() {
  quk <- dplyr::as_tibble(quk)
  ggplot2::ggplot(quk, ggplot2::aes(q, u)) + ggplot2::geom_point(ggplot2::aes(color = k, size = k)) +
    ggplot2::ggtitle("Flow-Speed Plot")
}
