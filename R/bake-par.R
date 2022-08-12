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

bake_par <- function(.x) {
  which <- "bake"
  nms <- parameter_names(which, use_tealeaves = FALSE)

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
  .x$Ds_gmc %<>% set_units(J / mol / K)
  .x$Ds_Jmax %<>% set_units(J / mol / K)
  .x$Ea_gammastar %<>% set_units(J / mol)
  .x$Ea_gmc %<>% set_units(J / mol)
  .x$Ea_Jmax %<>% set_units(J / mol)
  .x$Ea_KC %<>% set_units(J / mol)
  .x$Ea_KO %<>% set_units(J / mol)
  .x$Ea_Rd %<>% set_units(J / mol)
  .x$Ea_Vcmax %<>% set_units(J / mol)
  .x$Ea_Vtpu %<>% set_units(J / mol)
  .x$Ed_gmc %<>% set_units(J / mol)
  .x$Ed_Jmax %<>% set_units(J / mol)

  # Check values ----
  stopifnot(.x$Ds_gmc > set_units(0, J / mol / K))
  stopifnot(.x$Ds_Jmax > set_units(0, J / mol / K))
  stopifnot(.x$Ea_gammastar > set_units(0, J / mol))
  stopifnot(.x$Ea_gmc > set_units(0, J / mol))
  stopifnot(.x$Ea_Jmax > set_units(0, J / mol))
  stopifnot(.x$Ea_KC > set_units(0, J / mol))
  stopifnot(.x$Ea_KO > set_units(0, J / mol))
  stopifnot(.x$Ea_Rd > set_units(0, J / mol))
  stopifnot(.x$Ea_Vcmax > set_units(0, J / mol))
  stopifnot(.x$Ea_Vtpu > set_units(0, J / mol))
  stopifnot(.x$Ed_gmc > set_units(0, J / mol))
  stopifnot(.x$Ed_Jmax > set_units(0, J / mol))

  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
}
