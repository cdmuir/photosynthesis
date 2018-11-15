#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retreive: "leaf", "enviro", "temp", or "constants". Partial matching allowed.

parameter_names <- function(which) {
  
  which %>% 
    match.arg(c("leaf", "enviro", "temp", "constants")) %>%
    switch(
         leaf = c("g_ic", "g_sc", "g_uc", "g_xc", "gamma_star25",  "J_max25", 
                  "K_C25", "K_O25", "k_sc", "k_xc", "phi", "R_d25", "logit_sr", 
                  "T_leaf", "theta_J", "V_cmax25", "V_tpu25"),
         enviro = c("C_air", "O", "P", "PPFD"),
         temp = c("Ds_gm", "Ds_Jmax", "Ea_gammastar", "Ea_gm", "Ea_Jmax", 
                  "Ea_KC", "Ea_KO", "Ea_Rd", "Ea_Vcmax", "Ea_Vtpu", "Ed_gm", 
                  "Ed_Jmax"),
         constants = c("D_w0", "R", "s", "phi", "theta_J")
  )
  
}
