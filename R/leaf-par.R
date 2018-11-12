#' S3 class leaf_par
#' @exportClass leaf_par
#

#' @param .x A list to be constructed into \strong{leaf_par}.
#' 
#' @description 
#' 
#' Constructor function for leaf_par class. This function ensures that leaf parameter inputs are properly formatted.
#' 
#' @export

leaf_par <- function(.x) {
  
  which <- "leaf"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)
  
  # Check values ------
  stopifnot(.x$g_xc >= set_units(0, "mol / (m^2 * s * Pa)"))
  stopifnot(.x$g_ic >= set_units(0, "mol / (m^2 * s * Pa)"))
  stopifnot(.x$k_sc >= set_units(0))
  stopifnot(.x$k_xc >= set_units(0))
  stopifnot(.x$V_cmax >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$J_max >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$R_d >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$K_c >= set_units(0, "Pa"))
  stopifnot(.x$K_o >= set_units(0, "kPa"))
  stopifnot(.x$gamma_star >= set_units(0, "Pa"))
  stopifnot(.x$g_sw >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$k_sc == plogis(.x$logit_sr))
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

