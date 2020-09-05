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

ppm2pa <- function(ppm, P) {
  set_units(ppm * P, Pa)
}

#' Convert g_c (\eqn{\mu}mol CO2/m^2/s/Pa) to g_w (\eqn{\mu}mol H2O /m^2/s/Pa)
#'
#' @inheritParams bake
#' @param g_w conductance to water vapor in units
#' (\eqn{\mu}mol H2O / (m^2 s Pa)) of class \code{units}.
#' @param D_c diffusion coefficient for CO2 in air in units of m^2/s of call
#' \code{units}
#' @param D_w diffusion coefficient for H2O in air in units of m^2/s of call
#' \code{units}
#'
#' @return Value with units \eqn{\mu}mol / (m^2 s Pa) of class \code{units}.
#'
#' @details
#'
#' Diffusive conductance to CO2 is generally about ~1.6x that of H2O because of
#' the higher molecular weight. To convert, multiply conductance by the ratio
#' of diffusion coefficients:
#'
#' \deqn{g_\mathrm{c} = g_\mathrm{w} D_\mathrm{c} / D_\mathrm{w}}{g_c = g_w D_c / D_w}
#' \deqn{g_\mathrm{w} = g_\mathrm{c} D_\mathrm{w} / D_\mathrm{c}}{g_w = g_c D_w / D_c}
#'
#' @note This function will soon be moving to the standalone gunit package.
#'
#' @examples
#' D_c <- set_units(1.29e-05, "m^2/s")
#' D_w <- set_units(2.12e-05, "m^2/s")
#' g_c <- set_units(3, "umol/m^2/s/Pa")
#' g_w <- gc2gw(g_c, D_c, D_w, unitless = FALSE)
#' g_w
#'
#' gw2gc(g_w, D_c, D_w, unitles = FALSE)
#' @export
#'

gw2gc <- function(g_w, D_c, D_w, unitless) {
  if (unitless) {
    if (is(g_w, "units")) g_w %<>% drop_units()
    if (is(D_c, "units")) D_c %<>% drop_units()
    if (is(D_w, "units")) D_w %<>% drop_units()
    g_c <- g_w * D_c / D_w
    return(g_c)
  } else {
    g_w %<>% set_units("umol/m^2/s/Pa")
    D_c %<>% set_units("m^2/s")
    D_w %<>% set_units("m^2/s")
    g_c <- g_w * D_c / D_w
    return(g_c)
  }
}

#' Convert g_c (umol CO2/m^2/s/Pa) to g_w (umol H2O /m^2/s/Pa)
#'
#' @rdname gw2gc
#'
#' @inheritParams gw2gc
#' @param g_c conductance to CO2 in units (\eqn{\mu}mol H2O / (m^2 s Pa)) of
#' class \code{units}.
#' @export
#'

gc2gw <- function(g_c, D_c, D_w, unitless) {
  if (unitless) {
    if (is(g_c, "units")) g_c %<>% drop_units()
    if (is(D_c, "units")) D_c %<>% drop_units()
    if (is(D_w, "units")) D_w %<>% drop_units()
    g_w <- g_c * D_w / D_c
    return(g_w)
  } else {
    g_c %<>% set_units(umol / m^2 / s / Pa)
    D_c %<>% set_units(m^2 / s)
    D_w %<>% set_units(m^2 / s)
    g_w <- g_c * D_w / D_c
    return(g_w)
  }
}
