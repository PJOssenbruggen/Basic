#' The function \code{speedtsplot} plots the data from the \code{speedts} data frame.
#'
#' @param speedts, a matrix
#' @usage speedtsplot(speedts)
speedtsplot <- function(speedts) {
  speedts <- dplyr::as_tibble(speedts)
  ggplot2::ggplot(speedts, ggplot2::aes_string("t", "u", "day")) + ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, na.rm = TRUE) +
    ggplot2::ggtitle("Time-Speed Plot")
}


