#' The \code{ubreakdown} function generates a breakdown speed vector \code{ubk} assuming a lognormal distribution.
#'
#' @param nveh number of vehicles in the investigation or \code{vehicle} = {1, 2, ..., \code{nveh}}, a number
#' @param ubk average breakdown speed, a number
#' @param ubk.sd standard deviation of \code{ubk}, a number
#' @usage ubreakdown(nveh, ubk, ubk.sd)
# #' @examples
# #' ubreakdown(6, 10, 1)
ubreakdown <- function(nveh, ubk, ubk.sd) {
  omega <- sqrt(1 + log(ubk.sd^2)/(2 * log(ubk)))
  theta <- (2*log(ubk) - omega^2)/2
  ub    <- rlnorm(n = nveh, meanlog = theta, sdlog = omega)
  return(ub)
}
