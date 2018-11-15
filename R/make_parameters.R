#' Make lists of parameters for \code{photosynthesis}
#'
#' @param replace A named list of parameters to replace defaults. If \code{NULL}, defaults will be used.
#'
#' @name make_parameters
#' 
#' @encoding UTF-8

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @return 
#' 
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_temppar}: An object inheriting from class \code{\link{temp_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\mathrm{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{logit(sr)} \tab \code{logit_sr} \tab stomatal ratio (logit-scale) \tab none \tab 0 = logit(0.5) \cr
#' \eqn{g_\mathrm{xc}}{g_xc} \tab \code{g_xc} \tab intercellular conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 10 \cr
#' \eqn{g_\mathrm{ic}}{g_ic} \tab \code{g_ic} \tab intracellular conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 10 \cr
#' \eqn{k\mathrm{xc}}{k_xc} \tab \code{k_xc} \tab partition of \eqn{g_\mathrm{xc}}{g_xc} to spongy mesophyll \tab none \tab 1 \cr
#' \eqn{V_\mathrm{c,max25}}{V_c,max25} \tab \code{V_cmax25} \tab maximum rate of carboxylation (25 °C) \tab (mol CO2) / (m\eqn{^2} s) \tab 150 \cr
#' \eqn{V_\mathrm{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation (T_leaf) \tab (mol CO2) / (m\eqn{^2} s) \tab \link[=temper]{calculated} \cr
#' \eqn{V_\mathrm{tpu25}}{V_tpu25} \tab \code{V_tpu25} \tab rate of triose phosphate utilisation (25 °C) \tab (mol CO2) / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{V_\mathrm{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilisation (T_leaf) \tab (mol CO2) / (m\eqn{^2} s) \tab \link[=temper]{calculated} \cr
#' \eqn{J_\mathrm{max25}}{J_max25} \tab \code{J_max25} \tab potential electron transport (25 °C) \tab (mol CO2) / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab (mol CO2) / (m\eqn{^2} s) \tab \link[=temper]{calculated} \cr
#' \eqn{R_\mathrm{d25}}{R_d25} \tab \code{R_d25} \tab Nonphotorespiratory CO2 release (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2 \cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab Nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=temper]{calculated} \cr
#' \eqn{K_\mathrm{C25}}{K_C25} \tab \code{K_C25} \tab Michaelis constant for carboxylation (25 °C) \tab \eqn{\mu}mol / mol \tab 268.3\cr
#' \eqn{K_\mathrm{C}}{K_C} \tab \code{K_C} \tab Michaelis constant for carboxylation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=temper]{calculated} \cr
#' \eqn{K_\mathrm{O25}}{K_O25} \tab \code{K_O25} \tab Michaelis constant for oxygenation (25 °C) \tab \eqn{\mu}mol / mol \tab 165084.2\cr
#' \eqn{K_\mathrm{O}}{K_O} \tab \code{K_O} \tab Michaelis constant for oxygenation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=temper]{calculated} \cr
#' \eqn{\Gamma_25*} \tab \code{gamma_star25} \tab Chloroplastic CO2 compensation point (25 °C) \tab Pa \tab 3.743 \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab Chloroplastic CO2 compensation point (T_leaf) \tab Pa \tab \link[=temper]{calculated} \cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab 298.15
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{C_\mathrm{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246 \cr
#' PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab umol quanta / (m^2 s) \tab 1500
#' }
#'
#' \bold{Temperature response parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{D_\mathrm{s,gm}}{Ds_gm} \tab \code{Ds_gm} \tab empirical temperature response parameter \tab J / (mol K) \tab 487.29 \cr
#' \eqn{D_\mathrm{s,Jmax}}{Ds_Jmax} \tab \code{Ds_Jmax} \tab empirical temperature response parameter \tab J / (mol K) \tab 388.04 \cr
#' \eqn{E_\mathrm{a,\Gamma *}}{Ea_gammastar} \tab \code{Ea_gammastar} \tab empirical temperature response parameter \tab J / mol \tab 24459.97 \cr
#' \eqn{E_\mathrm{a,gm}}{Ea_gm} \tab \code{Ea_gm} \tab empirical temperature response parameter \tab J / mol \tab 68901.56 \cr
#' \eqn{E_\mathrm{a,Jmax}}{Ea_Jmax} \tab \code{Ea_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 56095.18 \cr
#' \eqn{E_\mathrm{a,KC}}{Ea_KC} \tab \code{Ea_KC} \tab empirical temperature response parameter \tab J / mol \tab 80989.78 \cr
#' \eqn{E_\mathrm{a,KO}}{Ea_KO} \tab \code{Ea_KO} \tab empirical temperature response parameter \tab J / mol \tab 23719.97 \cr
#' \eqn{E_\mathrm{a,Rd}}{Ea_Rd} \tab \code{Ea_Rd} \tab empirical temperature response parameter \tab J / mol \tab 40446.75 \cr
#' \eqn{E_\mathrm{a,Vcmax}}{Ea_Vcmax} \tab \code{Ea_Vcmax} \tab empirical temperature response parameter \tab J / mol \tab 52245.78 \cr
#' \eqn{E_\mathrm{d,gm}}{Ed_gm} \tab \code{Ed_gm} \tab empirical temperature response parameter \tab J / mol \tab 148788.56 \cr
#' \eqn{E_\mathrm{d,Jmax}}{Ed_Jmax} \tab \code{Ed_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 121244.79
#' }
#' 
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\phi} \tab \code{phi} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
#' \eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825 \cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08\cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598\cr
#' \eqn{D_{w,0}}{D_w0} \tab \code{D_w0} \tab diffusion coefficient for water vapour in air at 0 C \tab m\eqn{^2} / s \tab 21.2\cr
#' }
#'
#' * see manual for further detail on calculation
#'
#' @references 
#' 
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic rate into contributions from different variables. Plant, Cell & Environment 38: 1200-11.
#' 
#' @export

make_leafpar <- function(replace = NULL) {

  # Defaults -----
  obj <- list(
    g_ic = set_units(10, "umol / (m^2 * s * Pa)"), # CHECK DEFAULT in Pa^-1
    g_sc = set_units(4, "umol / (m^2 * s * Pa)"), # CHECK DEFAULT in Pa^-1
    g_uc = set_units(0.1, "umol / (m^2 * s * Pa)"), # CHECK DEFAULT in Pa^-1
    g_xc = set_units(10, "umol / (m^2 * s * Pa)"), # CHECK DEFAULT in Pa^-1
    k_xc = set_units(1),
    V_cmax25 = set_units(150, "umol / (m^2 * s)"),
    V_tpu25 = set_units(200, "umol / (m^2 * s)"),
    J_max25 = set_units(200, "umol / (m^2 * s)"),
    theta_J = set_units(0.825),
    phi = set_units(0.331),
    R_d25 = set_units(2, "umol / (m^2 * s)"),
    K_C25 = set_units(27.238, "Pa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    K_O25 = set_units(16.582, "kPa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    gamma_star25 = set_units(3.743, "Pa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    logit_sr = set_units(0),
    T_leaf = set_units(298.15, "K")
  )
  
  obj$k_sc <- plogis(obj$logit_sr)

  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% leaf_par()

  obj

}

#' make_enviropar
#' @rdname make_parameters
#' @export


make_enviropar <- function(replace = NULL) {

  # Defaults -----
  obj <- list(
    PPFD = set_units(1500, "umol/m^2/s"),
    C_air = set_units(41, "Pa"),
    P = set_units(101.3246, "kPa"),
    O = set_units(21.27565, "kPa")
  ) 
  
  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% enviro_par()
  
  obj

}

#' make_tempers
#' @rdname make_parameters
#' @export


make_temppar <- function(replace = NULL) {
  
  # Defaults -----
  obj <- list(
    Ds_gm = set_units(487.29, "J/mol/K"),
    Ds_Jmax = set_units(388.04, "J/mol/K"),
    Ea_gammastar = set_units(24459.97, "J/mol"),
    Ea_gm = set_units(68901.56, "J/mol"),
    Ea_Jmax = set_units(56095.18, "J/mol"),
    Ea_KC = set_units(80989.78, "J/mol"),
    Ea_KO = set_units(23719.97, "J/mol"),
    Ea_Rd = set_units(40446.75, "J/mol"), 
    Ea_Vcmax = set_units(52245.78, "J/mol"),
    Ea_Vtpu = set_units(52245.78, "J/mol"),
    Ed_gm = set_units(148788.56, "J/mol"),
    Ed_Jmax = set_units(121244.79, "J/mol")
  ) 
  
  # Replace defaults -----
  obj %<>% replace_defaults(replace)
  
  # Assign class and return -----
  obj %<>% temp_par()
  
  obj
  
}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants <- function(replace = NULL) {

  # Defaults -----
  obj <- list(
    phi = set_units(0.331),
    theta_J = set_units(0.825),
    s = set_units(5.67e-08, "W / (m ^ 2 * K ^ 4)"),
    R = set_units(8.3144598, "J / (mol * K)"),
    D_w0 = set_units(21.2e-6, "m ^ 2 / s")
  )

  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% constants()
  
  obj

}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#'

replace_defaults <- function(obj, replace) {

  if (!is.null(replace)) {

    stopifnot(is.list(replace))
    stopifnot(all(sapply(replace, inherits, what = "units")))
    stopifnot(all(sapply(replace, is.numeric)))
    x <- names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not recognized:\n%s", paste0(x[!x %in% names(obj)], collapse = "\n")))
      x %<>% .[. %in% names(obj)]
    }
    obj[x] <- replace[x]

  }

  obj

}
