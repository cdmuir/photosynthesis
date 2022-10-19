#' S3 class leaf_par
#

#' @inheritParams photosynthesis
#' @param .x A list to be constructed into \strong{leaf_par}.
#'
#' @returns 
#'
#' Constructor function for leaf_par class. This function ensures that leaf
#' parameter inputs are properly formatted.
#'
#' @export
leaf_par = function(.x, use_tealeaves) {
  which = "leaf"
  nms = photosynthesis::parameter_names(which, use_tealeaves)

  stopifnot(is.list(.x))

  if (!all(nms %in% names(.x))) {
    nms[!(nms %in% names(.x))] %>%
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} not in parameter names required for {which}",
        x = ., which = which
      ) %>%
      stop()
  }

  .x %<>% magrittr::extract(nms)

  # Set units ----
  # Message about change of conductance units in version 2.0.4
  check_for_legacy_gunit(.x)
  
  .x$g_mc25 %<>% set_units(mol / m^2 / s)
  .x$g_sc %<>% set_units(mol / m^2 / s)
  .x$g_uc %<>% set_units(mol / m^2 / s)
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
  stopifnot(.x$g_mc25 >= set_units(0, umol / m^2 / s))
  stopifnot(.x$g_sc >= set_units(0, umol / m^2 / s))
  stopifnot(.x$g_uc >= set_units(0, umol / m^2 / s))
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

check_for_legacy_gunit = function(pars) {
  
  pars |>
    purrr::map(units) |>
    purrr::map_lgl(units::ud_are_convertible, y = "umol / m^2 / s / Pa") |>
    any() %>%
    purrr::when(. ~ {
      
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
    })
  
  invisible()
  
}
