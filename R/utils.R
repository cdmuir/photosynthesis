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

  set_units(ppm * P, "Pa")
  
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
#' @export
#' 

convert_conductance <- function(.g, Temp = NULL, P = NULL) {
  
  stopifnot(inherits(.g, "units"))
  if (!is.null(Temp)) Temp %<>% set_units("K")
  if (!is.null(P)) P %<>% set_units("kPa")
  
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
      `umol/m^2/s/Pa` = set_units(.g / (R * Temp), "umol/m^2/s/Pa"),
      `mol/m^2/s` = set_units(.g * P / (R * Temp), "mol/m^2/s")
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
      `m/s` = set_units(.g * (R * Temp), "m/s"),
      `umol/m^2/s/Pa` = .g,
      `mol/m^2/s` = set_units(.g * P, "mol/m^2/s")
    )
    
  }
  
  # Convert from "mol/m^2/s" to others
  if (length(g_unit$numerator) == 1L &
      stringr::str_detect(g_unit$numerator, "mol$") &
      length(g_unit$denominator) > 1L &
      length(which(stringr::str_detect(g_unit$denominator, "m$"))) == 2L &
      !any(stringr::str_detect(g_unit$denominator, "Pa$")) &
      length(which(g_unit$denominator %in% c("s", "min", "hr"))) == 1L) {
    
    .g %<>% set_units("mol/m^2/s")
    ret <- list(
      `m/s` = set_units(.g * R * Temp / P, "m/s"),
      `umol/m^2/s/Pa` = set_units(.g / P, "umol/m^2/s/Pa"),
      `mol/m^2/s` = .g
    )
    
  }
  
  if (is.null(ret)) {
    warning("Units of .g did not match acceptable units. Returning NULL.")
    return(NULL)
  }  
  
  ret
  
}