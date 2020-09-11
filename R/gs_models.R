#' Stomatal conductance models
#'
#' @param A_net Net CO2 assimilation in umol m-2 s-1
#' @param C_air CO2 concentration at the leaf surface in umol mol-1
#' @param RH Relative humidity as a proportion
#'
#' @param D0 Vapor pressure sensitivity of stomata (Leuning 1995)
#' @param VPD Vapor pressure deficit (kPa)
#'
#' @param g0 Optimization model intercept term (Medlyn et al. 2011)
#' @param g1 Optimization model slope term (Medlyn et al. 2011)
#' @param gk Optimization model root term (Medlyn et al. 2011)
#'
#'
#' @return gs_mod_ballberry is used for fitting the Ball et al. (1987) model
#' of stomatal conductance
#'
#' gs_mod_leuning is used for fitting the Leuning (1995) model
#' of stomatal conductance
#'
#' gs_mod_opti fits the optimal stomatal conductance model according to
#' Medlyn et al. 2011
#'
#' gs_mod_optifull fits the full optimal stomatal conductance model according
#' to Medlyn et al. 2011
#'
#' @references
#'
#' Ball JT, Woodrow IE, Berry JA. 1987. A model predicting stomatal
#' conductance and its contribution to the control of photosynthesis
#' under different environmental conditions, in Progress in
#' Photosynthesis Research, Proceedings of the VII International
#' Congress on Photosynthesis, vol. 4, edited by I. Biggins, pp.
#' 221–224, Martinus Nijhoff, Dordrecht, Netherlands.
#'
#' Leuning R. 1995. A critical appraisal of a coupled stomatal-
#' photosynthesis model for C3 plants. Plant Cell Environ 18:339-357
#'
#' Medlyn BE, Duursma RA, Eamus D, Ellsworth DS, Prentice IC, Barton
#' CVM, Crous KY, Angelis PD, Freeman M, Wingate L. 2011. Reconciling
#' the optimal and empirical approaches to modeling stomatal
#' conductance. Glob Chang Biol 17:2134-2144
#'
#' @rdname gs_models
#' @export
gs_mod_ballberry <- function(A_net,
                             C_air,
                             RH) {
  A_net * C_air * RH
}
#' @rdname gs_models
#' @export
gs_mod_leuning <- function(A_net,
                           C_air,
                           D0,
                           VPD) {
  A_net / (C_air * (1 + VPD * D0))
}
#' @rdname gs_models
#' @export
gs_mod_opti <- function(g0,
                        g1,
                        VPD,
                        A_net,
                        C_air) {
  g0 + 1.6 * (1 + g1 / sqrt(VPD)) * (A_net / C_air)
}
#' @rdname gs_models
#' @export
gs_mod_optifull <- function(g0,
                            g1,
                            gk,
                            VPD,
                            A_net,
                            C_air) {
  g0 + 1.6 * (1 + g1 / VPD^(1 - gk)) * (A_net / C_air)
}
