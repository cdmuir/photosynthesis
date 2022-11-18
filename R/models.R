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
#' @inheritParams fit_photosynthesis
#' 
#' @return A character string with name of model.
#' 
#' @examples 
#' get_default_model("aq_response")
#' get_default_model("r_light")
#' 
#' @md
#' @export
get_default_model = function(.photo_fun) {
  .photo_fun = match.arg(.photo_fun, get_function_types())
  switch(
    .photo_fun,
    aq_response = "marshall_biscoe_1980",
    r_light = "walker_ort_2015"
  )
}

#' @rdname models
#' @export
get_all_models = function(method) {
  match.arg(method, get_function_types())
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
#' @inheritParams fit_photosynthesis
#' @export
required_variables = function(.model, quiet) {
  
  .model = match.arg(.model, get_function_types() |>
                       purrr::map(get_all_models) |>
                       unlist())
  all_vars = list(
    .A = "net CO2 assimilation rate (umol/m^2/s)",
    .C = "intercellular or chloroplastic CO2 concentration (umol/mol)",
    .phiPSII = "quantum efficiency of PSII electron transport (mol / mol)",
    .Q = "irradiance (umol/m^2/s)"
  )
  
  model_vars = switch(
    .model,
    kok_1956 = all_vars[c(".A", ".Q")],
    marshall_biscoe_1980 = all_vars[c(".A", ".Q")],
    walker_ort_2015 = all_vars[c(".A", ".C", ".Q")],
    yin_etal_2011 = all_vars[c(".A", ".phiPSII", ".Q")]
  ) 
  
  if (!quiet) {
    cat(.model, "\n")
    purrr::iwalk(model_vars, ~ {cat(.y, ": ", .x, "\n")})
  }
  
  invisible(names(model_vars))
  
}

#' Vector of method types
#' @noRd
get_function_types = function() {
  c("aq_response", "r_light")
}
