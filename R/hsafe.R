#' Safe headway \code{h} between lead and following vehicles. Speed \code{u} of lead vehicle is measured in mph. \code{leff} = 14 feet.
#'
#' @param u speed in units of fps, a number
#' @param leff effective vehicle length in feet, a number
#' @usage  hsafe(u, leff)
# #' @examples
# #' hsafe(60, 14)
#' @export
hsafe = function(u, leff) {
  mph = u * 3600 / 5280
  h = (1 + mph/10) * leff
  if(h < 2*leff) h = 2 * leff else h = (1 + mph/10) * leff
  return(h)
}
