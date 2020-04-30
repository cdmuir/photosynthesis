#' Ball-Berry model of stomatal conductance
#'
#' @param A_net Net CO2 assimilation in umol m-2 s-1
#' @param Ca CO2 concentration at the leaf surface in umol mol-1
#' @param RH Relative humidity as a proportion
#'
#' @return gs_mod_ballberry is used for fitting the Ball et al. (1987) model
#' of stomatal conductance
#' 
#' Ball JT, Woodrow IE, Berry JA. 1987. A model predicting stomatal 
#' conductance and its contribution to the control of photosynthesis
#' under different environmental conditions, in Progress in 
#' Photosynthesis Research, Proceedings of the VII International 
#' Congress on Photosynthesis, vol. 4, edited by I. Biggins, pp. 
#' 221–224, Martinus Nijhoff, Dordrecht, Netherlands.
#' @export
#' 
gs_mod_ballberry <- function(A_net,
                             Ca,
                             RH){
  A_net * Ca * RH
}
