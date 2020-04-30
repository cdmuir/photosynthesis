#' Peaked Arrhenius temperature response
#'
#' @param Tleaf Leaf temperature in Celsius
#' @param Ea Activation energy in J mol-1
#' @param Hd Deactivation energy in J mol-1
#' @param Topt Optimum temperature of the process in Celsius
#'
#' @return t_response_arrhenius_topt fits a peaked Arrhenius temperature 
#' response function
#' 
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' @export
t_response_arrhenius_topt <- function(Tleaf, Ea, Hd, Topt){
  Hd * exp(Ea * ( (Tleaf + 273.15) - (Topt + 273.15)) / 
             ( (Tleaf + 273.15) * (Topt + 273.15) * 8.314)) /
    (Hd - Ea * (1 - exp(Hd * ( (Tleaf + 273.15) - (Topt + 273.15)) / 
                          ( (Tleaf + 273.15) * (Topt + 273.15) * 8.314))))
}
