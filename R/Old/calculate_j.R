#' Non-rectangular hyperbola model for J
#'
#' @param Q_inc light intensity in umol m-2 s-1
#' @param alpha initial slope of the light response
#' @param Jmax maximum rate of electron transport in umol m-2 s-1
#' @param theta curvature of the light response
#'
#' @return calculate_j provides a model of the light response of J.
#' It is necessary for fitting the electron transport component
#' of the photosynthetic CO2 response curves in fit_aci_response.
#' @export
calculate_j <- function(Q_inc, 
                  alpha, 
                  Jmax, 
                  theta){
  (alpha * Q_inc + Jmax - 
     sqrt( (alpha * Q_inc + Jmax) ^ 2 - 4 * alpha * theta * Q_inc*Jmax)) / 
    (2 * theta)
}
