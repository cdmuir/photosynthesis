#' Leuning model of stomatal conductance
#'
#' @param A_net Net CO2 assimilation in umol m-2 s-1
#' @param Ca CO2 concentration at the leaf surface in umol mol-1
#' @param D0 Vapor pressure sensitivity of stomata
#' @param VPD Vapor pressure deficit (kPa)
#'
#' @return gs_mod_leuning is used for fitting the Leuning (1995) model
#' of stomatal conductance
#' 
#' Leuning R. 1995. A critical appraisal of a coupled stomatal-
#' photosynthesis model for C3 plants. Plant Cell Environ 18:339-357
#' @export
#' 
gs_mod_leuning <- function(A_net,
                             Ca,
                             D0,
                             VPD){
  A_net / (Ca * (1 + VPD * D0))
}
