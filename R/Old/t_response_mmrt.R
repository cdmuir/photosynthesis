#' Macromolecular rate theory temperature response function
#'
#' @param Tleaf Leaf temperature in Celsius
#' @param dH Change in enthalpy of the reaction at 25 C in J mol-1
#' @param dCp Change in heat capacity of the enzyme between the enzyme-substrate
#' and enzyme-transition states in J mol-1 K-1
#' @param dG Change in Gibbs free energy of the reaction at 25 C in J mol-1
#'
#' @return t_response_mmrt fits a macromolecular rate theory temperature response
#' according to Hobbs et al. 2013. Change in heat capacity for enzyme catalysis
#' determines temperature dependence of enzyme catalyzed rates. ACS Chemical
#' Biology 8:2388-2393
#' @export
t_response_mmrt <- function(dCp,
                            dG,
                            dH,
                            Tleaf){
  (log(1.380649e-23 *(298.15) / 6.62607e-34)) -
  dG / (8.314 * 298.15) + 
    (1 / 298.15 + dH / (8.314 * 298.15 ^ 2)) * ((Tleaf + 273.15) - 298.15) +
    (dCp / (2 * 8.314 * 298.15 ^ 2)) * ((Tleaf + 273.15) - 298.15) ^ 2
}
