#' Get vector of parameter names
#' 
#' @inheritParams photosynthesis
#' @param which A character string indicating which parameter names to retreive: "leaf", "enviro", "bake", or "constants". Partial matching allowed.
#' 
#' @examples 
#' parameter_names("leaf")
#' 
#' @export

parameter_names <- function(which, use_tealeaves) {
  
  bakepar_names <- c("Ds_gmc", "Ds_Jmax", "Ea_gammastar", "Ea_gmc", "Ea_Jmax", 
                     "Ea_KC", "Ea_KO", "Ea_Rd", "Ea_Vcmax", "Ea_Vtpu", "Ed_gmc", 
                     "Ed_Jmax")
  
  constants_names <- c("D_c0", "D_h0", "D_m0", "D_w0", "epsilon", "eT", "G",
                       "nu_constant", "R", "s", "sh_constant")
  
  enviropar_names <- c("C_air", "O", "P", "PPFD", "RH", "T_air", "wind")
  
  leafpar_names <- c("g_mc25", "g_sc", "g_uc", "gamma_star25", "J_max25", 
                     "K_C25", "K_O25", "k_mc", "k_sc", "k_uc", "leafsize", 
                     "phi_J", "R_d25", "T_leaf", "theta_J", "V_cmax25", 
                     "V_tpu25")
  
  if (use_tealeaves) {

    which %>% 
      match.arg(c("bake", "constants", "enviro", "leaf")) %>%
      switch(
        bake = bakepar_names,
        constants = constants_names,
        enviro = sort(c(enviropar_names, "E_q", "f_par", "S_sw")),
        leaf = sort(c(leafpar_names, "abs_l", "abs_s"))
      ) %>%
      return()
    
  } else {
    
    which %>% 
      match.arg(c("bake", "constants", "enviro", "leaf")) %>%
      switch(
        bake = bakepar_names,
        constants = constants_names,
        enviro = enviropar_names,
        leaf = leafpar_names
      ) %>%
      return()
    
  }
  
}
