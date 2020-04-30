#' Optimization model of stomatal conductance
#'
#' @param g0 Optimization model intercept term
#' @param g1 Optimization model slope term
#' @param VPD Vapour pressure deficit in kPa
#' @param A_net Net CO2 assimilation in mu*mol m-2 s-1
#' @param Ca CO2 concentration at the leaf surface in umol mol-1
#'
#' @return gs_mod_opti fits the optimal stomatal conductance model according to
#' Medlyn et al. 2011.
#' 
#' Medlyn BE, Duursma RA, Eamus D, Ellsworth DS, Prentice IC, Barton
#' CVM, Crous KY, Angelis PD, Freeman M, Wingate L. 2011. Reconciling
#' the optimal and empirical approaches to modelling stomatal 
#' conductance. Glob Chang Biol 17:2134-2144
#' @export
gs_mod_opti <- function(g0, 
                      g1, 
                      VPD, 
                      A_net, 
                      Ca){
  g0 + 1.6 * ( 1 + g1 / sqrt(VPD)) * (A_net / Ca)
}
