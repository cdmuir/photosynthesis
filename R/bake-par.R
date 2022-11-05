#' S3 class bake_par
#

#' @param .x A list to be constructed into \strong{bake_par}.
#'
#' @returns
#'
#' Constructor function for bake_par class. This function ensures that leaf
#' temperature gets properly "baked" into leaf parameters.
#'
#' @export

bake_par = function(.x) {
  
  which = "bake"
  
  # Check parameters names ----
  nms = check_parameter_names(.x, which = which, use_tealeaves = FALSE)
  .x = .x[nms]
  
  # Set units ----
  .x = .x |>
    set_parameter_units(
      .data$type == which, 
      !.data$temperature_response,
      !.data$tealeaves
    )
  
  # Assert bounds on values ----
  .x |>
    assert_parameter_bounds(
      .data$type == which, 
      !.data$temperature_response,
      !.data$tealeaves
    )
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}
