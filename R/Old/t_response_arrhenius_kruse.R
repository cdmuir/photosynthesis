#' Peaked Arrhenius temperature response function
#'
#' @param T2 Leaf temperature term
#' @param Ea_ref Activation energy in J mol-1
#' @param dEa Temperature-dependent change in Ea in K^2
#' @param Par_ref Parameter at reference temperature of 25 Celsius
#'
#' @return t_response_arrhenius_kruse fits a peaked Arrhenius response according to 
#' Kruse et al. 2008. Three parameters comprehensively describe the temperature
#' response of respiratory oxygen reduction. Plant Cell Environment 31:954-967
#' 
#' Kruse J, Adams MA. 2008. Three parameters comprehensively describe
#' the temperature response of respiratory oxygen reduction. Plant 
#' Cell Environ 31:954-967
#' @export
t_response_arrhenius_kruse <- function(dEa, Ea_ref, Par_ref, T2){
  log(Par_ref) + (Ea_ref / 8.314) * T2 + dEa * T2 ^ 2
}
