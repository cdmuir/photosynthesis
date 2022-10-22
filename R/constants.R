#' S3 class constants
#' @inheritParams photosynthesis
#' @param .x A list to be constructed into \strong{constants}.
#'
#' @returns
#' Constructor function for constants class. This function ensures that
#' physical constant inputs are properly formatted.
#'
#' @export
constants = function(.x, use_tealeaves) {
  
  which = "constants"
  
  # Check parameters names ----
  nms = check_parameter_names(.x, which = which, use_tealeaves = use_tealeaves)
  .x %<>% 
    magrittr::extract(nms) |>
    # Set units ----
  set_parameter_units(
    type == which, 
    !temperature_response,
    if (!use_tealeaves) {!tealeaves} else TRUE
  )
  
  # Assert bounds on values ----
  stopifnot(.x$D_c0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_h0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_m0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_w0 >= set_units(0, m^2 / s))
  stopifnot(.x$epsilon >= set_units(0))
  stopifnot(.x$eT >= set_units(0))
  stopifnot(.x$G >= set_units(0, m / s^2))
  stopifnot(.x$R >= set_units(0, J / (mol * K)))
  stopifnot(.x$sigma >= set_units(0, W / (m^2 * K^4)))
  # Additional parameters for using tealeaves ----
  if (use_tealeaves) {
    stopifnot(.x$c_p >= set_units(0, J / g / K))
    stopifnot(.x$R_air >= set_units(0, J / K / kg))
  }
  structure(.x, class = c(which, "list"))
}
