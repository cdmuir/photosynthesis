#' Make lists of parameters for \code{photosynthesis}
#'
#' @inheritParams photosynthesis
#' @param replace A named list of parameters to replace defaults. If \code{NULL}, defaults will be used.
#'
#' @name make_parameters

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @return 
#' 
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\text{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{logit(sr)} \tab \code{logit_sr} \tab stomatal ratio (logit-scale) \tab none \tab 0 = logit(0.5) \cr
#' \eqn{g_\text{xc}}{g_xc} \tab \code{g_xc} \tab intercellular conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 10 \cr
#' \eqn{g_\text{ic}}{g_ic} \tab \code{g_ic} \tab intracellular conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 10 \cr
#' \eqn{k\mathrm{xc}{k_xc} \tab \code{k_xc} \tab partition of \eqn{g_\text{xc}}{g_xc} to spongy mesophyll \tab none \tab 1\cr
#' \eqn{V_\text{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation \tab (mol CO2) / (m\eqn{^2} s) \tab 50\cr
#' \eqn{V_\text{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilisation \tab (mol CO2) / (m\eqn{^2} s) \tab ??? \cr
#' \eqn{J_\text{max}}{J_max} \tab \code{J_max} \tab potential electron transport \tab (mol CO2) / (m\eqn{^2} s) \tab 100\cr
#' \eqn{R_\text{d}}{R_d} \tab \code{R_d} \tab Nonphotorespiratory CO2 release \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2\cr
#' \eqn{K_\text{C}}{K_C} \tab \code{K_c} \tab Michaelis constant for carboxylation \tab \eqn{\mu}mol / mol \tab 268.3\cr
#' \eqn{K_\text{O}}{K_O} \tab \code{K_o} \tab Michaelis constant for oxygenation \tab \eqn{\mu}mol / mol \tab 165084.2\cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab Chloroplastic CO2 compensation point \tab Pa \tab 3.743
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{T_\text{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab 298.15 \cr
#' \eqn{C_\text{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246 \cr
#' \eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
#' }
#'
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\{phi}} \tab \code{phi} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
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
    V_cmax = set_units(50, "umol / (m^2 * s)"),
    V_tpu = set_units(200, "umol / (m^2 * s)"),
    J_max = set_units(100, "umol / (m^2 * s)"),
    theta_J = set_units(0.825),
    phi = set_units(0.331),
    R_d = set_units(2, "umol / (m^2 * s)"),
    K_c = set_units(27.238, "Pa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    K_o = set_units(16.582, "kPa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    gamma_star = set_units(3.743, "Pa"), # From Sharkey et al. 2007. Newer source? Check bayCi
    logit_sr = set_units(0)
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

  ##### Defaults -----
  obj <- list(
    PPFD = set_units(1500, "umol/m^2/s"),
    T_leaf = set_units(298.15, "K"),
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
