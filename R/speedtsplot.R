#' The function \code{speedtsplot} plots the data from the \code{speedts} data frame.
#'
#' @usage speedtsplot()
speedtsplot <- function() {
  speedts <- dplyr::as_tibble(speedts)
  ggplot2::ggplot(speedts, ggplot2::aes(t, u, day)) + ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, na.rm = TRUE) +
    ggplot2::ggtitle("Time-Speed Plot")
}


