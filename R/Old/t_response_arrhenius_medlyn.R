#' Peaked Arrhenius temperature response function
#'
#' @param Tleaf Leaf temperature in Celsius
#' @param Ea Activation energy in J mol-1
#' @param Hd Deactivation energy in J mol-1
#' @param dS Entropy parameter in J mol-1
#'
#' @return t_response_arrhenius_medlyn fits a peaked Arrhenius response as found in
#' Medlyn et al. 2002. Temperature response of parameters of a biochemically
#' based model of photosynthesis. II. A review of experimental data. Plant
#' Cell Environment 25:1167-1179
#' 
#' @export
t_response_arrhenius_medlyn <- function(Tleaf, Ea, Hd, dS){
  exp(Ea * ( (Tleaf + 273.15) - 298.15) / (298.15 * 8.314 * (Tleaf + 273.15))) * 
    (1 + exp( (298.15 * dS - Hd)/(298.15 * 8.314))) / 
    (1 + exp( ((Tleaf + 273.15) * dS - Hd)/( (Tleaf + 273.15) * 8.314)))
}
