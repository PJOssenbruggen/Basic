#' Pre-breakdown and breakdown speed data from I-93 in Salem, New Hampshire in 2010.
#'
#' A data frame of nortbound traffic speed observations that are aggregated over fifteen minute intervals.
#' The pre-breakdown speeds are shown as at times \code{t} = -1. The breakdown speeds are show at times \code{t} = 0,1,2,... .
#'
#' @format A data frame with 8697 rows and 2 columns:
#' \describe{
#' \item{u}{traffic speed, miles per hour (mph)}
#' \item{t}{time index}
#' }
"I93speed"
