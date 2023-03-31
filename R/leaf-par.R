#' S3 class leaf_par
#

#' @inheritParams photosynthesis
#' @param .x A list to be constructed into **leaf_par**.
#'
#' @returns 
#'
#' Constructor function for leaf_par class. This function ensures that leaf
#' parameter inputs are properly formatted.
#'
#' @export
leaf_par = function(.x, use_tealeaves) {
  
  which = "leaf"
  
  # Message about change of conductance units in version 2.1.0
  check_for_legacy_gunit(.x)
  
  # Check parameters names ----
  nms = check_parameter_names(.x, which = which, use_tealeaves = use_tealeaves)
  .x = .x |> 
    magrittr::extract(nms) |>
    # Set units ----
  set_parameter_units(
    .data$type == "leaf", 
    !.data$temperature_response,
    if (!use_tealeaves) {!.data$tealeaves} else TRUE
  )
  
  # Assert bounds on values ----
  .x |>
    assert_parameter_bounds(
      .data$type == which, 
      !.data$temperature_response,
      if (!use_tealeaves) {!.data$tealeaves} else TRUE
    )
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

check_for_legacy_gunit = function(pars) {
  
  xx = pars |>
    purrr::map(units) |>
    purrr::map_lgl(units::ud_are_convertible, y = "umol / m^2 / s / Pa") |>
    any()
  
    if (xx) {
      
      message(
        "
      It looks like one or more of the conductance values is provided in units 
      of 'umol / m^2 / s / Pa' or a unit that can be converted to it. This was 
      the default in photosynthesis (<= 2.0.3), but we switched to 
      'mol / m^2 / s' because these units are morely widely used in plant 
      ecophysiology. 
      
      To convert, use this code with your desired temperature and pressure:
      g = units::set_units(1, umol / m^2 / s / Pa)   
      P = units::set_units(101.3246, kPa)
      Temp = set_units(298.15, K) 
      gunit::convert_conductance(g, P = P, Temp = Temp)$`mol/m^2/s`
      
      "
      )
      stop("Incorrect conductance units in leaf_par()", call. = FALSE)
    }
  
  invisible()
  
}
