#' The function \code{passplot} plots the data from \code{brksummary}, \code{freeflowpass}, \code{decelmerge} data frames.
#'
#' @param df a matrix
#' @param title a character string
#' @usage passplot(df, title)
# #' @examples
# #' passplot(df, "Merge")
#' @export
passplot <- function(df, title) {
#  if(!is.factor(df[,5])) lane <- factor(df[,5])
  df <- dplyr::as_tibble(df)
  ggplot2::ggplot(df, ggplot2::aes_string("y", "x")) +
  ggplot2::geom_point(ggplot2::aes_string(size = "u", color = "u")) +
  ggplot2::geom_point(ggplot2::aes_string(shape = "lane")) +
  ggplot2::ggtitle(title)
}
