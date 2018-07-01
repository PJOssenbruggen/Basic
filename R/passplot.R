#' The function \code{passplot} plots the data from \code{brksummary}, \code{freeflowpass}, \code{decelmerge} data frames.
#'
#' @param df from \code{freeflowpass}, a matrix
#' @param title, a character string
#' @usage passplot(df, title)
# #' examples
# #' passplot(df, title = "Free-Flow Pass and Merge")
#' @export
passplot <- function(df, title) {
#  df      %>% dplyr::bind_cols(df)
  speedts <- dplyr::as_tibble(df)
  ggplot2::ggplot(speedts, ggplot2::aes_string("y", "x")) +
  ggplot2::geom_point(ggplot2::aes_string(size = "u", color = "u")) +
  ggplot2::geom_point(ggplot2::aes(shape = factor(lane))) +
  ggplot2::ggtitle(title)
#   ggplot2::geom_point(ggplot2::aes_string(shape = factor("lane")))
}
