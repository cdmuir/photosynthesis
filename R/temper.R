#' S3 class tempered
#' @exportClass tempered
#

#' 'temper' leaf parameters using temperature response functions
#' 
#' @inheritParams photosynthesis
#
#' @description 
#' 
#' Constructor function for \code{tempered} class. This will also inherit class \code{\link{leaf_par}}. This function ensures that leaf parameter are "tuned" to \code{T_leaf} using temperature response functions detailed below. 
#' 
#' @details 
#' 
#' Temperature response functions following Bernacchi et al. 2002. \cr
#' \cr
#' Temperature response function 1 (\code{temp_response1}): \cr
#' 
#' \deqn{par(T_\mathrm{leaf}) = par25 exp(E_\mathrm{a} / (R T_\mathrm{ref}) (T_\mathrm{leaf} - 25) / (T_\mathrm{leaf} + 273.15))}{par(T_leaf) = par25 exp(E_a / (R T_ref) (T_leaf - 25) / (T_leaf + 273.15))}
#' 
#' \eqn{T_\mathrm{ref}}{T_ref} is the reference temperature in K \cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} is the leaf temperature in °C \cr
#' \cr
#' Temperature response function 2 (\code{temp_response2}) is the above equation multiplied by: \cr
#' 
#' \deqn{(1 + exp((D_\mathrm{s} / R - E_\mathrm{d} / (R T_\mathrm{ref})))) / (1 + exp((D_\mathrm{s} / R) - (E_\mathrm{d} / (R (T_\mathrm{leaf} + 273.15)))))}{(1 + exp((D_s / R - E_d / (R T_ref)))) / (1 + exp((D_s / R) - (E_d / (R (T_leaf + 273.15)))))}
#' 
#' @encoding UTF-8
#' 
#' @references 
#' 
#' Bernacchi CJ, Portis AR, Nakano H, von Caemmerer S, Long SP. 2002. Temperature response of mesophyll conductance. Implications for the determination of Rubisco enzyme kinetics and for limitations to photosynthesis in vivo. Plant Physiology 130: 1992-8.
#' 
#' @examples 
#' leaf_par <- make_leafpar(replace = list(T_leaf = set_units(293.15, "K")))
#' temp_par <- make_temppar()
#' constants <- make_constants()
#' tempered_leafpar <- temper(leaf_par, temp_par, constants)
#' 
#' tempered_leafpar$V_cmax25
#' tempered_leafpar$V_cmax
#' 
#' @export

temper <- function(leaf_par, temp_par, constants) {
  
  leaf_par %<>% leaf_par()
  temp_par %<>% temp_par()
  constants %<>% constants()
  
  pars <- c(leaf_par, temp_par, constants)
  T_ref <- set_units(298.15, "K")
  
  leaf_par$gamma_star <- temp_resp1(pars$gamma_star, pars$Ea_gammastar, pars$R, 
                                    pars$T_leaf, T_ref)
  leaf_par$J_max <- temp_resp2(pars$J_max25, pars$Ds_Jmax, pars$Ea_Jmax, 
                               pars$Ed_Jmax, pars$R, pars$T_leaf, T_ref)
  leaf_par$K_C <- temp_resp1(pars$K_C25, pars$Ea_KC, pars$R, pars$T_leaf, T_ref)
  leaf_par$K_O <- temp_resp1(pars$K_O25, pars$Ea_KO, pars$R, pars$T_leaf, T_ref)
  leaf_par$R_d <- temp_resp1(pars$R_d25, pars$Ea_Rd, pars$R, pars$T_leaf, T_ref)
  leaf_par$V_cmax <- temp_resp1(pars$V_cmax25, pars$Ea_Vcmax, pars$R, 
                                pars$T_leaf, T_ref)
  leaf_par$V_tpu <- temp_resp1(pars$V_tpu25, pars$Ea_Vtpu, pars$R, pars$T_leaf, 
                               T_ref)

  # Set units ----
  leaf_par$gamma_star %<>% set_units("Pa")
  leaf_par$J_max %<>% set_units("umol / (m^2 * s)")
  leaf_par$K_C %<>% set_units("Pa")
  leaf_par$K_O %<>% set_units("kPa")
  leaf_par$R_d %<>% set_units("umol / (m^2 * s)")
  leaf_par$V_cmax %<>% set_units("umol / (m^2 * s)")
  leaf_par$V_tpu %<>% set_units("umol / (m^2 * s)")

  # Check values ----
  stopifnot(leaf_par$gamma_star >= set_units(0, "Pa"))
  stopifnot(leaf_par$J_max >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(leaf_par$K_C >= set_units(0, "Pa"))
  stopifnot(leaf_par$K_O >= set_units(0, "kPa"))
  stopifnot(leaf_par$R_d >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(leaf_par$V_cmax >= set_units(0, "umol / (m^2 * s)"))
  stopifnot(leaf_par$V_tpu >= set_units(0, "umol / (m^2 * s)"))
  
  leaf_par %<>% structure(class = c("tempered", "leaf_par", "list"))
  
  leaf_par
  
}

#' Temperature response function 1
#' 
#' @rdname temper
#' 
#' @param par25 Parameter value at 25 °C of class \code{units}.
#' @param E_a Empirical temperature response value in J/mol of class \code{units}.
#' @param R Ideal gas constant in J / (mol K) of class \code{units}. See \code{\link{make_constants}}.
#' @param T_leaf Leaf temperature in K of class \code{units}. Will be converted to °C.
#' @param T_ref Reference temperature in K of class \code{units}.
#' 
#' @export

temp_resp1 <- function(par25, E_a, R, T_leaf, T_ref) {
 
  pars_unit <- units(par25)
  par25 %<>% drop_units()

  E_a %<>% set_units("J/mol") %>% drop_units()
  R %<>% set_units("J/K/mol") %>% drop_units()
  T_leaf %<>% set_units("degreeC") %>% drop_units()
  T_ref %<>% set_units("K") %>% drop_units()
  
  a1 <- exp(E_a / (R * T_ref) * ((T_leaf - 25) / (T_leaf + 273.15)))
  
  ret <- par25 * a1
  units(ret) <- pars_unit
  ret
  
}

#' Temperature response function 2
#' 
#' @rdname temper
#' 
#' @inheritParams temp_resp1
#' @param D_s Empirical temperature response value in J / (mol K) of class \code{units}.
#' @param E_d Empirical temperature response value in J/mol of class \code{units}.
#' 
#' @export

temp_resp2 <- function(par25, D_s, E_a, E_d, R, T_leaf, T_ref) {
  
  a1 <- temp_resp1(par25, E_a, R, T_leaf, T_ref)
  pars_unit <- units(par25)
  par25 %<>% drop_units()
  a1 %<>% drop_units()
  
  D_s %<>% set_units("J/mol/K") %>% drop_units()
  E_a %<>% set_units("J/mol") %>% drop_units()
  E_d %<>% set_units("J/mol") %>% drop_units()
  R %<>% set_units("J/K/mol") %>% drop_units()
  T_leaf %<>% set_units("degreeC") %>% drop_units()
  T_ref %<>% set_units("K") %>% drop_units()
  
  a2 <- (1 + exp((D_s / R - E_d / (R * T_ref)))) / 
    (1 + exp((D_s / R) - (E_d / (R * (T_leaf + 273.15)))))
  
  ret <- a1 * a2
  units(ret) <- pars_unit
  ret
  
}
