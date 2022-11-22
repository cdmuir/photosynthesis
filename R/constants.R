#' S3 class constants
#' @inheritParams photosynthesis
#' @param .x A list to be constructed into **constants**.
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
  .x = .x |> 
    magrittr::extract(nms) |>
    # Set units ----
  set_parameter_units(
    .data$type == which, 
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
  
    structure(.x, class = c(which, "list"))
    
}
