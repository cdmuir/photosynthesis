#' Fitting a 2nd order polynomial
#'
#' @param a 2nd order coefficient
#' @param b 1st order coefficient
#' @param c Intercept
#' @param Tleaf Leaf temperature in Celsius
#'
#' @return t_response_quad is used to fit a 2nd order polynomial
#' @export
t_response_quad <- function(a,
                            b,
                            c,
                            Tleaf){
  a * Tleaf ^ 2 + b * Tleaf + c
}
