#' Convert pressure from PPM to Pascals
#'
#' @param ppm Pressure value in umol/mol of class \code{units}
#' @param P Atmospheric pressure value in kPa of class \code{units}
#'
#' @return Value in Pa of class \code{units}
#'
#' @details
#'
#' \deqn{\mathrm{Press}(kPa) = \mathrm{Press}(ppm) P(kPa)}{Press(kPa) = Press(ppm) P(kPa)}
#' \deqn{\mathrm{Press}(Pa) = 1000 \mathrm{Press}(kPa)}{Press(Pa) = 1000 Press(kPa)}
#'
#' @examples
#'
#' ppm <- set_units(400, "umol/mol")
#' P <- set_units(101.325, "kPa")
#' ppm2pa(ppm, P)
#' @export
#'

ppm2pa = function(ppm, P) {
  set_units(ppm * P, Pa)
}
