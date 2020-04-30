#' Inverse non-rectangular hyperbola for Jmax calculation
#'
#' @param Q_inc light intensity in umol m-2 s-1
#' @param alpha initial slope of the light response
#' @param J electron transport rate in umol m-2 s-1
#' @param theta curvature of the light response
#'
#' @return calculate_jmax calculates Jmax given Q_inc and J.
#' It is necessary for the electron transport component of the
#' fit_aci_response function.
#' @export
calculate_jmax <- function(Q_inc, 
                          alpha, 
                          J, 
                          theta){
  J * (J * theta - alpha * Q_inc) / (J - alpha * Q_inc)
}
