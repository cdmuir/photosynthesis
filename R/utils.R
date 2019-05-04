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
#' 
#' @export
#' 

ppm2pa <- function(ppm, P) {

  set_units(ppm * P, Pa)
  
}


#' Convert conductance units
#' 
#' @param .g Conductance in class units. Units must convertible to one of "m/s", "umol/m^2/s/Pa", or "mol/m^2/s"
#' @param Temp A temperature value of class \code{units}
#' @param P A pressure value of class \code{units} that is convertible to kPa
#' 
#' @return A list of three values of clas \code{units} with units "m/s", "umol/m^2/s/Pa", and "mol/m^2/s".
#' 
#' @examples 
#' 
#' g_sc <- set_units(10, "m/s")
#' convert_conductance(g_sc, 
#'                     Temp = set_units(298.15, "K"), 
#'                     P = set_units(101.3246, "kPa"))
#' 
#' g_sc <- set_units(4, "umol/m^2/s/Pa")
#' convert_conductance(g_sc, 
#'                     Temp = set_units(298.15, "K"), 
#'                     P = set_units(101.3246, "kPa"))
#' 
#' g_sc <- set_units(0.4, "mol/m^2/s")
#' convert_conductance(g_sc, 
#'                     Temp = set_units(298.15, "K"), 
#'                     P = set_units(101.3246, "kPa"))
#' 
#' @note This function will soon be moving to the standalone gunit package.
#' 
#' @export
#' 

convert_conductance <- function(.g, Temp = NULL, P = NULL) {
  
  stopifnot(inherits(.g, "units"))
  if (!is.null(Temp)) Temp %<>% set_units(K)
  if (!is.null(P)) P %<>% set_units(kPa)
  
  R <- make_constants()$R # ideal gas constant
  g_unit <- units(.g)
  
  # Convert from "m/s" to others
  if (length(g_unit$numerator) == 1L &
      stringr::str_detect(g_unit$numerator, "m$") &
      length(g_unit$denominator) == 1L &
      any(g_unit$denominator %in% c("s", "min", "hr"))) {
    
    .g %<>% set_units("m/s")
    ret <- list(
      `m/s` = .g,
      `umol/m^2/s/Pa` = set_units(.g / (R * Temp), umol/m^2/s/Pa),
      `mol/m^2/s` = set_units(.g * P / (R * Temp), mol/m^2/s)
    )
    
  }
  
  # Convert from "umol/m^2/s/Pa" to others
  if (length(g_unit$numerator) == 1L &
      stringr::str_detect(g_unit$numerator, "mol$") &
      length(g_unit$denominator) > 1L &
      length(which(stringr::str_detect(g_unit$denominator, "m$"))) == 2L &
      length(which(stringr::str_detect(g_unit$denominator, "Pa$"))) == 1L &
      length(which(g_unit$denominator %in% c("s", "min", "hr"))) == 1L) {
    
    .g %<>% set_units("umol/m^2/s/Pa")
    ret <- list(
      `m/s` = set_units(.g * (R * Temp), m/s),
      `umol/m^2/s/Pa` = .g,
      `mol/m^2/s` = set_units(.g * P, mol/m^2/s)
    )
    
  }
  
  # Convert from "mol/m^2/s" to others
  if (length(g_unit$numerator) == 1L &
      stringr::str_detect(g_unit$numerator, "mol$") &
      length(g_unit$denominator) > 1L &
      length(which(stringr::str_detect(g_unit$denominator, "m$"))) == 2L &
      !any(stringr::str_detect(g_unit$denominator, "Pa$")) &
      length(which(g_unit$denominator %in% c("s", "min", "hr"))) == 1L) {
    
    .g %<>% set_units(mol/m^2/s)
    ret <- list(
      `m/s` = set_units(.g * R * Temp / P, m/s),
      `umol/m^2/s/Pa` = set_units(.g / P, umol/m^2/s/Pa),
      `mol/m^2/s` = .g
    )
    
  }
  
  if (is.null(ret)) {
    warning("Units of .g did not match acceptable units. Returning NULL.")
    return(NULL)
  }  
  
  ret
  
}

#' Convert g_c (\eqn{\mu}mol CO2/m^2/s/Pa) to g_w (\eqn{\mu}mol H2O /m^2/s/Pa)
#'
#' @inheritParams bake
#' @param g_w conductance to water vapor in units (\eqn{\mu}mol H2O / (m^2 s Pa)) of class \code{units}.
#' @param D_c diffusion coefficient for CO2 in air in units of m^2/s of call \code{units}
#' @param D_w diffusion coefficient for H2O in air in units of m^2/s of call \code{units}
#'
#' @return Value with units \eqn{\mu}mol / (m^2 s Pa) of class \code{units}.
#' 
#' @details 
#' 
#' Diffusive conductance to CO2 is generally about ~1.6x that of H2O because of the higher molecular weight. To convert, multiply conductance by the ratio of diffusion coefficients:
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
#' 
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
#' @param g_c conductance to CO2 in units (\eqn{\mu}mol H2O / (m^2 s Pa)) of class \code{units}.
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
    g_c %<>% set_units("umol/m^2/s/Pa")
    D_c %<>% set_units("m^2/s")
    D_w %<>% set_units("m^2/s")
    g_w <- g_c * D_w / D_c
    return(g_w)
  }
  
}
