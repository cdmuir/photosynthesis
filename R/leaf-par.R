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
  
  # Set units ----
  .x$g_ic %<>% set_units("umol / (m^2 * s * Pa)")
  .x$g_sc %<>% set_units("umol / (m^2 * s * Pa)")
  .x$g_xc %<>% set_units("umol / (m^2 * s * Pa)")
  .x$g_uc %<>% set_units("umol / (m^2 * s * Pa)")
  .x$k_sc %<>% set_units()
  .x$k_xc %<>% set_units()
  .x$V_cmax25 %<>% set_units("umol / (m^2 * s)")
  .x$V_tpu25 %<>% set_units("umol / (m^2 * s)")
  .x$J_max25 %<>% set_units("umol / (m^2 * s)")
  .x$theta_J %<>% set_units()
  .x$phi %<>% set_units()
  .x$R_d25 %<>% set_units("umol / (m^2 * s)")
  .x$K_C25 %<>% set_units("Pa")
  .x$K_O25 %<>% set_units("kPa")
  .x$gamma_star25 %<>% set_units("Pa")
  .x$T_leaf %<>% set_units("K")
  
  # Check values ----
  stopifnot(.x$g_ic >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$g_sc >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$g_xc >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$g_uc >= set_units(0, "umol / (m^2 * s * Pa)"))
  stopifnot(.x$k_sc >= set_units(0) & .x$k_sc == plogis(.x$logit_sr))
  stopifnot(.x$k_xc >= set_units(0))
  stopifnot(.x$V_cmax25 >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$V_tpu25 >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$J_max25 >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$theta_J > set_units(0) & .x$theta_J < set_units(1))
  stopifnot(.x$phi > set_units(0))
  stopifnot(.x$R_d25 >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(.x$K_C25 >= set_units(0, "Pa"))
  stopifnot(.x$K_O25 >= set_units(0, "kPa"))
  stopifnot(.x$gamma_star25 >= set_units(0, "Pa"))
  stopifnot(.x$T_leaf >= set_units(0, "K"))
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

