#' S3 class constants
#' @inheritParams photosynthesis
#' @param .x A list to be constructed into \strong{constants}.
#'
#' @description
#' Constructor function for constants class. This function ensures that
#' physical constant inputs are properly formatted.
#'
#' @export
constants <- function(.x, use_tealeaves) {
  which <- "constants"
  nms <- parameter_names(which, use_tealeaves)
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
  .x$D_c0 %<>% set_units(m^2 / s)
  .x$D_h0 %<>% set_units(m^2 / s)
  .x$D_m0 %<>% set_units(m^2 / s)
  .x$D_w0 %<>% set_units(m^2 / s)
  .x$epsilon %<>% set_units()
  .x$eT %<>% set_units()
  .x$G %<>% set_units(m / s^2)
  .x$R %<>% set_units(J / (mol * K))
  .x$s %<>% set_units(W / (m^2 * K^4))
  # Check values ----
  stopifnot(.x$D_c0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_h0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_m0 >= set_units(0, m^2 / s))
  stopifnot(.x$D_w0 >= set_units(0, m^2 / s))
  stopifnot(.x$epsilon >= set_units(0))
  stopifnot(.x$eT >= set_units(0))
  stopifnot(.x$G >= set_units(0, m / s^2))
  stopifnot(.x$R >= set_units(0, J / (mol * K)))
  stopifnot(.x$s >= set_units(0, W / (m^2 * K^4)))
  # Additional parameters for using tealeaves ----
  if (use_tealeaves) {
    .x$c_p %<>% set_units(J / g / K)
    .x$R_air %<>% set_units(J / K / kg)
    stopifnot(.x$c_p >= set_units(0, J / g / K))
    stopifnot(.x$R_air >= set_units(0, J / K / kg))
  }
  structure(.x, class = c(which, "list"))
}
