#' Calculating dS from the Topt model
#'
#' @param Ea Activation energy in J/mol
#' @param Hd Deactivation energy in J/mol
#' @param Topt Optimum temperature
#'
#' @return t_response_calc_dS calculates dS from the fitted Topt
#' model.
#' @export
t_response_calc_dS <- function(Ea,
                               Hd,
                               Topt){
  Hd / Topt + 8.314 * log(Ea / (Hd - Ea))
}