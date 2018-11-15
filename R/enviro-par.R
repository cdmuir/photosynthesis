#' S3 class enviro_par
#' @exportClass enviro_par
#

#' @param .x A list to be constructed into \strong{enviro_par}.
#' 
#' @description 
#' 
#' Constructor function for enviro_par class. This function ensures that environmental parameter inputs are properly formatted.
#' 
#' @export

enviro_par <- function(.x) {
  
  which <- "enviro"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)
  
  # Set units ----
  .x$C_air %<>% set_units("Pa")
  .x$O %<>% set_units("kPa")
  .x$P %<>% set_units("kPa")
  .x$PPFD %<>% set_units("umol/m^2/s")
  
  # Check values ----
  stopifnot(.x$C_air >= set_units(0, "Pa") & .x$C_air <= .x$P)
  stopifnot(.x$O >= set_units(0, "kPa") & .x$O <= .x$P)
  stopifnot(.x$P >= set_units(0, "kPa"))
  stopifnot(.x$PPFD >= set_units(0, "umol/m^2/s"))
  
  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}

