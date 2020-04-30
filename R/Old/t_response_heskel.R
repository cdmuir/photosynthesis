#' Heskel-type temperature response function
#'
#' @param Tleaf Leaf temperature in Celsius
#' @param a Constant to minimize residuals
#' @param b Linear coefficient to minmize residuals
#' @param c Quadratic coefficient to minimize residuals
#'
#' @return t_response_heskel fits a temperature response according to
#' Heskel et al. 2016. Convergence in the temperature response of leaf respiration
#' across biomes and plant functional types. PNAS 113:3832-3837
#' @export
t_response_heskel <- function(Tleaf, a, b, c){
  a + b * (Tleaf) + c * (Tleaf) ^ 2
}
