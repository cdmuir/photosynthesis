#' Farquhar-von Caemmerer-Berry (FvCB) C3 photosynthesis model
#'
#' @inheritParams A_supply
#'
#' @return A list of four values with units umol CO2 / (m^2 s) of class \code{units}:
#' \cr
#' - \code{W_carbox}: Rubisco-limited assimilation rate \cr
#' - \code{W_regen}: RuBP regeneration-limited assimilation rate \cr
#' - \code{W_tpu}: TPU-limited assimilation rate \cr
#' - \code{A}: minimum of W_carbox, W_regen, and W_tpu
#'
#' @details
#'
#' Equations following Buckley and Diaz-Espejo (2015): \cr
#' \cr
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
#' \deqn{W_\mathrm{regen} = J C_\mathrm{chl} / (4 C_\mathrm{chl} + 8 \Gamma*)}{W_regen = J C_chl / (4 C_chl + 8 \Gamma*)}
#'
#' where \eqn{J} is a function of PPFD, obtained by solving the equation:
#'
#' \deqn{0 = \theta_J J ^ 2 - J (J_\mathrm{max} + \phi_J PPFD) + J_\mathrm{max} \phi_J PPFD}{0 = \theta_J J ^ 2 - J (J_max + \phi_J PPFD) + J_max \phi_J PPFD}
#'
#' \bold{TPU-limited assimilation rate:} \cr
#'
#' \deqn{W_\mathrm{tpu} = 3 V_\mathrm{tpu} C_\mathrm{chl} / (C_\mathrm{chl} - \Gamma*)}{W_tpu = 3 V_tpu C_chl / (C_chl - \Gamma*)}
#' \cr
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{C_\mathrm{chl}}{C_chl} \tab \code{C_chl} \tab chloroplastic CO2 concentration \tab Pa \tab input \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab chloroplastic CO2 compensation point (T_leaf) \tab Pa \tab \link[=bake]{calculated} \cr
#' \eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{K_\mathrm{C}}{K_C} \tab \code{K_C} \tab Michaelis constant for carboxylation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{K_\mathrm{O}}{K_O} \tab \code{K_O} \tab Michaelis constant for oxygenation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
#' \eqn{\phi_J} \tab \code{phi_J} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
#' PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab umol quanta / (m^2 s) \tab 1500 \cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825 \cr
#' \eqn{V_\mathrm{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{V_\mathrm{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilization (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated}
#' }
#'
#' @references
#'
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic
#' rate into contributions from different variables. Plant, Cell & Environment
#' 38: 1200-11.
#'
#' Farquhar GD, Caemmerer S, Berry JA. 1980. A biochemical model of
#' photosynthetic CO2 assimilation in leaves of C3 species. Planta 149: 78–90.
#'
#' @examples
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(use_tealeaves = FALSE)
#' leaf_par = bake(leaf_par, enviro_par, bake_par, constants)
#'
#' pars = c(leaf_par, enviro_par, constants)
#' C_chl = set_units(246.0161, umol / mol)
#' FvCB(C_chl, pars)
#' @export
#'
FvCB = function(C_chl, pars, unitless = FALSE) {
  ret = list(
    W_carbox = W_carbox(C_chl, pars, unitless),
    W_regen = W_regen(C_chl, pars, unitless),
    W_tpu = W_tpu(C_chl, pars, unitless)
  )

  # Ignore W_tpu if C_chl < gamma_star
  if (C_chl > pars$gamma_star) {
    ret$A = min(ret$W_carbox, ret$W_regen, ret$W_tpu)
  } else {
    ret$A = min(ret$W_carbox, ret$W_regen)
  }
  ret
}
#' Rubisco-limited assimilation rate
#' @rdname FvCB
#' @export
W_carbox = function(C_chl, pars, unitless = FALSE) {
  if (unitless) {
    A = pars$V_cmax * C_chl / (C_chl + pars$K_C * (1 + 1e6 * pars$O / pars$K_O))
  } else {
    A = set_units(
      pars$V_cmax * C_chl /
        (C_chl + pars$K_C * (set_units(1) + pars$O / pars$K_O)),
      umol / m^2 / s
    )
  }
  A
}
#' RuBP regeneration-limited assimilation rate
#' @rdname FvCB
#' @export
W_regen = function(C_chl, pars, unitless = FALSE) {
  J = J(pars, unitless)
  A = J * C_chl / (4 * C_chl + 8 * pars$gamma_star)
  if (!unitless) A %<>% set_units(umol / m^2 / s)
  A
}
#' TPU-limited assimilation rate
#' @rdname FvCB
#' @export
W_tpu = function(C_chl, pars, unitless = FALSE) {
  A = 3 * pars$V_tpu * C_chl / (C_chl - pars$gamma_star)
  if (!unitless) A %<>% set_units(umol / m^2 / s)
  A
}
#' J: Rate of electron transport (umol/m^2/s)
#'
#' @description Calculate the rate of electron transport as a function of photosynthetic photon flux density (PPFD).
#'
#' @inheritParams .get_gtc
#'
#' @return Value in \eqn{\mu}mol/ (m^2 s) of class \code{units}
#'
#' @details
#'
#' \eqn{J} as a function of PPFD is the solution to the quadratic expression:
#'
#' \deqn{0 = \theta_J J ^ 2 - J (J_\mathrm{max} + \phi_J PPFD) + J_\mathrm{max} \phi_J PPFD}{0 = \theta_J J ^ 2 - J (J_max + \phi_J PPFD) + J_max \phi_J PPFD}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{\phi_J} \tab \code{phi_J} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
#' PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab \eqn{\mu}mol quanta / (m^2 s) \tab 1500 \cr
#' \eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825
#' }
#'
#' @examples
#'
#' library(magrittr)
#' library(photosynthesis)
#'
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(use_tealeaves = FALSE)
#' enviro_par$T_air = leaf_par$T_leaf
#' leaf_par %<>% bake(enviro_par, bake_par, constants)
#'
#' pars = c(leaf_par, enviro_par, constants)
#' J(pars, FALSE)
#' @export
#'
J = function(pars, unitless = FALSE) {
  if (!unitless) {
    # drop units for root finding
    pars$PPFD %<>% set_units(umol / m^2 / s) %>% drop_units()
    pars$J_max %<>% set_units(umol / m^2 / s) %>% drop_units()
    pars$phi_J %<>% drop_units()
    pars$theta_J %<>% drop_units()
  }

  .f = function(J, PPFD, J_max, phi_J, theta_J) {
    theta_J * J^2 - J * (J_max + phi_J * PPFD) + J_max * phi_J * PPFD
  }

  J_I = stats::uniroot(.f, c(0, pars$J_max),
    PPFD = pars$PPFD, J_max =
      pars$J_max,
    phi_J = pars$phi_J, theta_J = pars$theta_J
  )

  J_I %<>% magrittr::use_series("root")

  if (!unitless) J_I %<>% set_units(umol / m^2 / s)

  J_I
}
