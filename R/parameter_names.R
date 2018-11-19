#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retreive: "leaf", "enviro", "bake", or "constants". Partial matching allowed.
#' 
#' @example parameter_names("leaf")
#' 
#' @export

parameter_names <- function(which) {
  
  which %>% 
    match.arg(c("leaf", "enviro", "bake", "constants")) %>%
    switch(
         leaf = c("g_mc25", "g_sc", "g_uc", "gamma_star25", "J_max25", 
                  "K_C25", "K_O25", "k_mc", "k_sc", "k_uc", "leafsize", "phi", 
                  "R_d25", "T_leaf", "theta_J", "V_cmax25", "V_tpu25"),
         enviro = c("C_air", "O", "P", "PPFD", "RH", "T_air", "wind"),
         bake = c("Ds_gmc", "Ds_Jmax", "Ea_gammastar", "Ea_gmc", "Ea_Jmax", 
                  "Ea_KC", "Ea_KO", "Ea_Rd", "Ea_Vcmax", "Ea_Vtpu", "Ed_gmc", 
                  "Ed_Jmax"),
         constants = c("D_c0", "D_h0", "D_m0", "epsilon", "eT", "G",
                       "nu_constant", "R", "s", "sh_constant")
  )
  
}
