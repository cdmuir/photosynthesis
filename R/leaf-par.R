#' S3 class leaf_par
#' @exportClass leaf_par
#

#' @inheritParams photosynthesis
#' @param .x A list to be constructed into \strong{leaf_par}.
#' 
#' @description 
#' 
#' Constructor function for leaf_par class. This function ensures that leaf parameter inputs are properly formatted.
#' 
#' @export

leaf_par <- function(.x, use_tealeaves) {
  
  which <- "leaf"
  nms <- photosynthesis::parameter_names(which, use_tealeaves)
  
  stopifnot(is.list(.x))
  
  if (!all(nms %in% names(.x))) {
    nms[!(nms %in% names(.x))] %>%
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} not in parameter names required for {which}",
                 x = ., which = which) %>%
      stop()
  }
  
  .x %<>% magrittr::extract(nms)
  
  # Set units ----
  .x$g_mc25 %<>% set_units(umol / (m^2 * s * Pa))
  .x$g_sc %<>% set_units(umol / (m^2 * s * Pa))
  .x$g_uc %<>% set_units(umol / (m^2 * s * Pa))
  .x$gamma_star25 %<>% set_units(Pa)
  .x$J_max25 %<>% set_units(umol / (m^2 * s))
  .x$k_mc %<>% set_units()
  .x$k_sc %<>% set_units()
  .x$k_uc %<>% set_units()
  .x$K_C25 %<>% set_units(Pa)
  .x$K_O25 %<>% set_units(kPa)
  .x$phi_J %<>% set_units()
  .x$R_d25 %<>% set_units(umol / (m^2 * s))
  .x$theta_J %<>% set_units()
  if (!use_tealeaves) .x$T_leaf %<>% set_units(K)
  .x$V_cmax25 %<>% set_units(umol / (m^2 * s))
  .x$V_tpu25 %<>% set_units(umol / (m^2 * s))
  
  # If using tealeaves, convert conductance values ----
  if (use_tealeaves) {
    .x$abs_l %<>% set_units()
    .x$abs_s %<>% set_units()
  }
  
  # Check values ----
  stopifnot(.x$g_mc25 >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(.x$g_sc >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(.x$g_uc >= set_units(0, umol / (m^2 * s * Pa)))
  stopifnot(.x$gamma_star25 >= set_units(0, Pa))
  stopifnot(.x$J_max25 >= set_units(0, umol / (m^2 * s)))
  stopifnot(.x$k_mc >= set_units(0))
  stopifnot(.x$k_sc >= set_units(0))
  stopifnot(.x$k_uc >= set_units(0))
  stopifnot(.x$K_C25 >= set_units(0, Pa))
  stopifnot(.x$K_O25 >= set_units(0, kPa))
  stopifnot(.x$phi_J > set_units(0))
  stopifnot(.x$theta_J > set_units(0) & .x$theta_J < set_units(1))
  stopifnot(.x$R_d25 >= set_units(0, umol / (m^2 * s)))
  if (!use_tealeaves) stopifnot(.x$T_leaf >= set_units(0, K))
  stopifnot(.x$V_cmax25 >= set_units(0, umol / (m^2 * s)))
  stopifnot(.x$V_tpu25 >= set_units(0, umol / (m^2 * s)))
  
  if (use_tealeaves) {
    stopifnot(.x$abs_l > set_units(0) & .x$abs_l < set_units(1))
    stopifnot(.x$abs_s > set_units(0) & .x$abs_s < set_units(1))
  }
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

