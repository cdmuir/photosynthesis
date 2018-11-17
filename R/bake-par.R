#' S3 class bake_par
#' @exportClass bake_par
#

#' @param .x A list to be constructed into \strong{bake_par}.
#' 
#' @description 
#' 
#' Constructor function for bake_par class. This function ensures that leaf temperature gets properly "baked" into leaf parameters.
#' 
#' @export

bake_par <- function(.x) {
  
  which <- "bake"
  nms <- parameter_names(which)
  
  stopifnot(is.list(.x))
  
  stopifnot(all(nms %in% names(.x)))
  
  .x %<>% magrittr::extract(nms)
  
  # Set units ----
  .x$Ds_gm %<>% set_units("J/mol/K")
  .x$Ds_Jmax %<>% set_units("J/mol/K")
  .x$Ea_gammastar %<>% set_units("J/mol")
  .x$Ea_gm %<>% set_units("J/mol")
  .x$Ea_Jmax %<>% set_units("J/mol")
  .x$Ea_KC %<>% set_units("J/mol")
  .x$Ea_KO %<>% set_units("J/mol")
  .x$Ea_Rd %<>% set_units("J/mol")
  .x$Ea_Vcmax %<>% set_units("J/mol")
  .x$Ea_Vtpu %<>% set_units("J/mol")
  .x$Ed_gm %<>% set_units("J/mol")
  .x$Ed_Jmax %<>% set_units("J/mol")
  
  # Check values ----
  stopifnot(.x$Ds_gm > set_units(0, "J/mol/K"))
  stopifnot(.x$Ds_Jmax > set_units(0, "J/mol/K"))
  stopifnot(.x$Ea_gammastar > set_units(0, "J/mol"))
  stopifnot(.x$Ea_gm > set_units(0, "J/mol"))
  stopifnot(.x$Ea_Jmax > set_units(0, "J/mol"))
  stopifnot(.x$Ea_KC > set_units(0, "J/mol"))
  stopifnot(.x$Ea_KO > set_units(0, "J/mol"))
  stopifnot(.x$Ea_Rd > set_units(0, "J/mol")) 
  stopifnot(.x$Ea_Vcmax > set_units(0, "J/mol"))
  stopifnot(.x$Ea_Vtpu > set_units(0, "J/mol"))
  stopifnot(.x$Ed_gm > set_units(0, "J/mol"))
  stopifnot(.x$Ed_Jmax > set_units(0, "J/mol"))

  structure(.x, class = c(stringr::str_c(which, "_par"), "list"))
  
}
