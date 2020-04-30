#' Arrhenius temperature response function
#'
#' @param Tleaf Leaf temperature in Celsius
#' @param Ea Activation energy in J mol-1
#'
#' @return t_response_arrhenius calculates the rate of a process based on an 
#' Arrhenius-type curve
#' 
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#' @export
t_response_arrhenius <- function(Tleaf, Ea){
  exp(Ea * ( (Tleaf + 273.15) - 298.15) / (298.15 * 8.314 * (Tleaf + 273.15)))
}
