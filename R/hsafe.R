#' Safe headway \code{h} between lead and following vehicles.
#'
#' @return A safe headway is defined by a following vehicle speed equal to or less than its leading vehicle speed and
#' distance headway that is one passenger car length for each 10 mph.
#' @param u speed in units of fps, a number
#' @param leff effective vehicle length in feet, a number
#' @usage  hsafe(u, leff)
#' @examples
#' hsafe(60, 14)
#' @export
hsafe = function(u, leff) {
  mph <- u * 3600 / 5280
  if(mph <= 10) h <- 2 * leff else h <- (1 + mph/10) * leff
  return(h)
}
