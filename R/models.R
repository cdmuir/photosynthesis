#' Get default model
#' 
#' @rdname models
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Get the name of the default model used for different plant ecophysiological data analysis methods implemented in \bold{photosynthesis}. Currently only used for \code{\link{fit_aq_response2}} and \code{\link{fit_r_light2}}.
#' 
#' @param method Character string. One of "aq_response", "r_light"
#' 
#' @return A character string with name of model.
#' 
#' @examples 
#' get_default_model("aq_response")
#' get_default_model("r_light")
#' 
#' @md
#' @export
get_default_model = function(method) {
  match.arg(method, get_method_types())
  switch(
    method,
    aq_response = "marshall_biscoe_1980",
    r_light = "walker_ort_2015"
  )
}

#' @rdname models
#' @export
get_all_models = function(method) {
  match.arg(method, get_method_types())
  switch(
    method,
    aq_response = "marshall_biscoe_1980",
    r_light = c("kok_1956", "walker_ort_2015", "yin_etal_2011")
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

#' Variables required for **photosynthesis** models
#' 
#' @param .model A character string of model name to use. See \code{\link{get_all_models}}. 
#' @export
required_variables = function(.model) {
  
  .model = match.arg(.model, get_method_types() |>
                       purrr::map(get_all_models) |>
                       unlist())
  all_vars = list(
    .A = "net CO2 assimilation rate (umol/m^2/s)",
    .Q = "irradiance (umol/m^2/s)"
  )
  
  cat(.model, "\n")
  switch(
    .model,
    kok_1956 = "TBA",
    marshall_biscoe_1980 = all_vars[c(".A", ".Q")],
    walker_ort_2015 = "TBA",
    yin_etal_2011 = "TBA"
  ) |>
    purrr::iwalk(~ {cat(.y, ": ", .x, "\n")})
  
  invisible()
  
}

#' Vector of method types
#' @noRd
get_method_types = function() {
  c("aq_response", "r_light")
}
