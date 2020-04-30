#' Calculating Topt from Arrhenius temperature responses
#'
#' @param Hd Deactivation energy in J mol-1
#' @param dS Entropy parameter in J mol-1
#' @param Ea Activation energy in J mol-1
#'
#' @return t_response_calc_topt calculates Topt for a process from Arrhenius
#' parameters
#' @export
t_response_calc_topt <- function(Hd, dS, Ea){
  Hd / (dS - 8.314 * log(Ea / (Hd - Ea)))
}
