#' Farquhar-von Caemmerer-Berry (FvCB) C3 photosynthesis model
#' 
#' @inheritParams A_supply
#' 
#' @return A list of four values with units umol CO2 / (m^2 s) of class \code{units}:
#' \cr
#' - \code{W_carbox}: Rubisco-limited assimilation rate
#' - \code{W_regen}: RuBP regeneration-limited assimilation rate
#' - \code{W_tpu}: TPU-limited assimilation rate
#' - \code{A}: hyperbolic average of W_carbox, W_regen, and W_tpu
#' 
#' @details 
#' 
#' \bold{Rubisco-limited assimilation rate:} \cr
#' \cr
#' \deqn{W_\mathrm{carbox} = V_\mathrm{c,max} C_\mathrm{chl} / (C_\mathrm{chl} + K_\mathrm{m})}{W_carbox = V_cmax C_chl / (C_chl + K_m)}
#' 
#' where:
#' 
#' \deqn{K_\mathrm{m} = K_\mathrm{C} (1 + O / K_\mathrm{O})}{K_m = K_c (1 + O / K_o)}
#' 
#' \bold{RuBP regeneration-limited assimilation rate:} \cr
#' \cr
#' \deqn{W_\mathrm{regen} = J (C_\mathrm{chl} - \Gamma*) / (4 C_\mathrm{chl} + 8 \Gamma)}{W_regen = J C_chl / (4 C_chl + 8 \Gamma*)}
#' 
#' where:
#' 
#' \deqn{J = \theta_J PAR ^ 2 - PAR J_\mathrm{max} + PPFD \phi) + J_\mathrm{max} PPFD \phi}{J = \theta_J PAR ^ 2 - PAR J_max + PPFD \phi) + J_max PPFD \phi}
#' 
#' \bold{TPU-limited assimilation rate:} \cr
#' \cr
#' \deqn{W_\mathrm{tpu} = 3 V_\mathrm{tpu} C_\mathrm{chl} / (C_\mathrm{chl} - \Gamma*)}{W_tpu = 3 V_tpu C_chl / (C_chl - \Gamma*)}
#'
#' @references 
#' 
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic rate into contributions from different variables. Plant, Cell & Environment 38: 1200-11.
#'  
#' Farquhar GD, Caemmerer S, Berry JA. 1980. A biochemical model of photosynthetic CO2 assimilation in leaves of C3 species. Planta 149: 78–90. 
#' 
#' @export
#' 

FvCB <- function(C_chl, pars) {
  
  ret <- list(
    W_carbox = W_carbox(C_chl, pars),
    W_regen = W_regen(C_chl, pars),
    W_tpu = W_tpu(C_chl, pars)
  )
  
  # Ignore W_tpu if C_chl < gamma_star
  if (C_chl > pars$gamma_star) {
    ret$A <- min(ret$W_carbox, ret$W_regen, ret$W_tpu)
  } else {
    ret$A <- min(ret$W_carbox, ret$W_regen)
  }
  
  ret
  
}

#' Rubisco-limited assimilation rate
#' @rdname FvCB
#' @export

W_carbox <- function(C_chl, pars) {
  
  set_units(pars$V_cmax * C_chl / (C_chl + pars$K_c * (set_units(1) + pars$O / pars$K_o)),
            "umol/m^2/s")
  
}

#' RuBP regeneration-limited assimilation rate
#' @rdname FvCB
#' @export

W_regen <- function(C_chl, pars) {
  
  J <- J(pars)
  
  A <- J * C_chl / (4 * C_chl + 8 * pars$gamma_star)
  
  A %<>% set_units("umol/m^2/s")
  
  A
  
}

#' TPU-limited assimilation rate
#' @rdname FvCB
#' @export

W_tpu <- function(C_chl, pars) {
  
  set_units(3 * pars$V_tpu * C_chl / (C_chl - pars$gamma_star), "umol/m^2/s")
  
}

#' J: Rate of electron transport (mol/m^2/s)
#' 
#' @inheritParams .get_gtc
#' 
#' @return Value in mol/ (m^2 s) of class \code{units}
#' 
#' @details 
#' 
#' \eqn{J} as a function of PPFD is the solution to the quadratic expression:
#' 
#' \deqn{0 = \theta_J J ^ 2 - J (J_\mathrm{max} + \phi PPFD) + J_\mathrm{max} \phi PPFD}{0 = \theta_J J ^ 2 - J (J_max + \phi PPFD) + J_max \phi PPFD}
#' 
#' @export
#' 
J <- function(pars) {
  
  # drop units for root finding
  PPFD <- pars$PPFD %<>% set_units("mol/m^2/s") %>% drop_units()
  J_max <- pars$J_max %<>% set_units("mol/m^2/s") %>% drop_units()
  phi <- pars$phi %<>% drop_units()
  theta_J <- pars$theta_J %<>% drop_units()
  
  .f <- function(J, PPFD, J_max, phi, theta_J) {
    
    theta_J * J ^ 2 - J * (J_max + phi * PPFD) + J_max * phi * PPFD

  }
  
  J_I <- stats::uniroot(.f, c(0, J_max), PPFD = PPFD, J_max = J_max, phi = phi, 
                        theta_J = theta_J)
  
  J_I %<>% 
    magrittr::use_series("root") %>%
    set_units("mol/m^2/s")
  
  J_I
  
}
