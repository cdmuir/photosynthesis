#' Non-rectangular hyperbolic model of light responses
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' Please use `marshall_biscoe_1980()`.
#' 
#' @param k_sat Light saturated rate of process k
#' @param phi_J Quantum efficiency of process k
#' @param Q_abs Absorbed light intensity (umol m-2 s-1)
#' @param theta_J Curvature of the light response
#'
#' @return aq_response is used to describe the response of a process to
#' absorbed light intensity. Assumes that input is absorbed light. Note
#' that if absorbed light is not used, then the meaning of phi_J becomes
#' unclear. This function is designed to be used with fit_aq_response,
#' however it could easily be fed into a different fitting approach (e.g.
#' Bayesian approaches). Originally from Marshall et al. 1980.
#'
#' @references
#' Marshall B, Biscoe P. 1980. A model for C3 leaves describing the
#' dependence of net photosynthesis on irradiance. J Ex Bot 31:29-39
#' @export
aq_response = function(k_sat, phi_J, Q_abs, theta_J) {
  
  lifecycle::deprecate_warn("2.1.1", "aq_response()", "marshall_biscoe_1980()", always = FALSE)
  
  k_net = ((k_sat + phi_J * Q_abs) -
    sqrt((k_sat + phi_J * Q_abs)^2 -
      4 * k_sat * phi_J * Q_abs * theta_J)) /
    (2 * theta_J)
}
