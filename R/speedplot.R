#' A scatter plot of traffic data.
#'
#' @param QUKtdayX, a matrix
#' @examples speedplot(QUKtdayX)
speedplot <- function(QUKtdayX) {
  quk0 <- QUKtdayX
  quk1 <- dplyr::as_tibble(quk0)
  ggplot2::ggplot(quk1, ggplot2::aes_string("q", "u")) +
    ggplot2::geom_point(ggplot2::aes_string(color = "k", size = "k")) +
    ggplot2::ggtitle("Flow-Speed Plot")
}
