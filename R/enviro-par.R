#' S3 class enviro_par
#

#' @inheritParams photosynthesis
#' @param .x A list to be constructed into \strong{enviro_par}.
#'
#' @returns 
#'
#' Constructor function for enviro_par class. This function ensures that environmental parameter inputs are properly formatted.
#'
#' @export

enviro_par = function(.x, use_tealeaves) {
  
  which = "enviro"
  
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
  
  # Notify about T_sky ----
  if (use_tealeaves) {

    # T_sky can be set or provided as a function ----
    if (is.null(.x$T_sky)) {
      message(
        '\nphotosynthesis (>= 1.0.2) will require users provide a T_sky value or
        function to calculate T_sky from other parameters.
        
        For back-compatibility, if T_sky is not provided, this warning will
        appear and the default function used in tealeaves (< 1.0.2) will be
        applied.
        
        See more details in vignette("parameter-functions")
        '
      )

      .x$T_sky = get_f_parameter("T_sky")
      
    } else {
      stopifnot(is.function(.x$T_sky) | is.double(.x$T_sky))

      if (is.double(.x$T_sky)) {
        .x$T_sky %<>% set_units(K)
      }
    }
  }

  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
}
