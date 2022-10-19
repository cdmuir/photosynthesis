#' Get vector of parameter names
#'
#' @inheritParams photosynthesis
#' @param which A character string indicating which parameter names to retrieve: "leaf", "enviro", "bake", or "constants". Partial matching allowed.
#'
#' @returns 
#' 
#' A character vector with parameter names associated with each type, "leaf", "enviro", "bake", or "constants".
#' 
#' @examples
#'
#' parameter_names("leaf", use_tealeaves = FALSE)
#' @export
parameter_names = function(which, use_tealeaves) {
  
  which |>
    match.arg(get_par_types()) |>
    make_default_parameter_list(use_tealeaves) |>
    names()

}
