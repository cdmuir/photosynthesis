#' Get vector of parameter names
#' 
#' @param which A character string indicating which parameter names to retreive, "leaf", "enviro", or "constants". Partial matching allowed.

parameter_names <- function(which) {
  
  which %>% 
    match.arg(c("leaf", "enviro", "constants")) %>%
    switch(
         leaf = c("g_ic", "g_sw", "g_uw", "g_xc", "gamma_star",  "J_max", "K_c", 
                  "K_o", "k_sc", "k_xc", "R_d", "logit_sr", "V_cmax"),
         enviro = c("C_air", "O", "P", "T_leaf"),
         constants = c("D_w0", "R", "s", "thetaJ")
  )
  
}
