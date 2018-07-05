#' The function \code{capacityplot} creates a \code{(k,Q*)} plot indicating the success rate of sustaining capacity \code{Q*}
#'
#' @param QUKtdayX a data frame
#' @usage capacityplot(QUKtdayX)
#' @examples
#' capacityplot(QUKtdayX)
#' @export
capacityplot <- function(QUKtdayX) {
  df <- cbind(uk_sdk(QUKtdayX, FALSE), logisticmodel(QUKtdayX, FALSE))
  df <- df[,c(2,3,8)]
  Q  <- df[,1] * df[,2]
  df <- cbind(df, Q)
  success  <- 1 - df[,3]
  df[,3] <- success
  colnames(df) <- c("k", "u", "Success", "Q")
  df <- dplyr::as_tibble(df)
  ggplot2::ggplot(df, ggplot2::aes_string("k", "Q")) +
    ggplot2::geom_point(ggplot2::aes_string(size = "Success", color = "Success")) +
    ggplot2::ggtitle("Capacity")
}
