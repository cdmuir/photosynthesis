#' Inverse non-rectangular hyperbola for J_max calculation
#'
#' @param PPFD light intensity in umol m-2 s-1
#' @param alpha initial slope of the light response
#' @param J electron transport rate in umol m-2 s-1
#' @param J_max maximum rate of electron transport in umol m-2 s-1
#' @param theta_J curvature of the light response
#'
#' @return calculate_jmax calculates J_max given PPFD and J.
#' It is necessary for the electron transport component of the
#' fit_aci_response function.
#'
#' calculate_j provides a model of the light response of J.
#' It is necessary for fitting the electron transport component
#' of the photosynthetic CO2 response curves in fit_aci_response.
#'
#' @rdname j_calculations
#' @export
calculate_jmax <- function(PPFD,
                           alpha,
                           J,
                           theta_J) {
  J * (J * theta_J - alpha * PPFD) / (J - alpha * PPFD)
}

#' @rdname j_calculations
#' @export
calculate_j <- function(PPFD,
                        alpha,
                        J_max,
                        theta_J) {
  (alpha * PPFD + J_max -
    sqrt((alpha * PPFD + J_max)^2 - 4 * alpha * theta_J * PPFD * J_max)) /
    (2 * theta_J)
}
