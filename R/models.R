#' Get default model
#' 
#' @rdname models
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Get the name of the default model used for different plant ecophysiological data analysis methods implemented in \bold{photosynthesis}. Currently only used for \code{\link{fit_aq_response2}}.
#' 
#' @param method Character string. One of "aq_response"
#' 
#' @return A character string with name of model.
#' 
#' @examples 
#' get_default_model("aq_response")
#' 
#' @md
#' @export
get_default_model = function(method) {
  match.arg(method, "aq_response")
  switch(
    method,
    aq_response = "marshall_biscoe_1980"
  )
}

#' @rdname models
#' @export
get_all_models = function(method) {
  match.arg(method, "aq_response")
  switch(
    method,
    aq_response = "marshall_biscoe_1980"
  )
}

#' @rdname models
#' @description
#' **Light response models:**
#' 
#' * `marshall_biscoe_1980()`: Non-rectangular hyperbolic model of light responses
#'
#' @param Q_abs Absorbed light intensity (\eqn{\mu}mol m\eqn{^{-2}} s\eqn{^{-1}})
#' @param k_sat Light saturated rate of process k
#' @param phi_J Quantum efficiency of process k
#' @param theta_J Curvature of the light response
#' 
#' @md
#' 
#' @export
marshall_biscoe_1980 = function(Q_abs, k_sat, phi_J, theta_J) {
  
  ((k_sat + phi_J * Q_abs) - 
     sqrt((k_sat + phi_J * Q_abs) ^ 2 - 4 * k_sat * phi_J * Q_abs * theta_J)) / 
  (2 * theta_J)

}