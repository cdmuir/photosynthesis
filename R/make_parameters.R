#' Make lists of parameters for \code{photosynthesis}
#'
#' @param replace A named list of parameters to replace defaults.
#' If \code{NULL}, defaults will be used.
#'
#' @name make_parameters
#'
#' @encoding UTF-8

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @inheritParams photosynthesis
#'
#' @return
#'
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_bakepar}: An object inheriting from class \code{\link{bake_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{D_{c,0}}{D_c0} \tab \code{D_c0} \tab diffusion coefficient for CO2 in air at 0 °C \tab m\eqn{^2} / s \tab 1.29e-5 \cr
#' \eqn{D_{h,0}}{D_h0} \tab \code{D_h0} \tab diffusion coefficient for heat in air at 0 °C \tab m\eqn{^2} / s \tab 1.90e-5 \cr
#' \eqn{D_{m,0}}{D_m0} \tab \code{D_m0} \tab diffusion coefficient for momentum in air at 0 °C \tab m\eqn{^2} / s \tab 1.33e-5 \cr
#' \eqn{D_{w,0}}{D_w0} \tab \code{D_w0} \tab diffusion coefficient for water vapor in air at 0 °C \tab m\eqn{^2} / s \tab 2.12e-5 \cr
#' \eqn{\epsilon} \tab \code{epsilon} \tab ratio of water to air molar masses \tab none \tab 0.622 \cr
#' \eqn{G} \tab \code{G} \tab gravitational acceleration \tab m / s\eqn{^2} \tab 9.8 \cr
#' \eqn{eT} \tab \code{eT} \tab exponent for temperature dependence of diffusion \tab none \tab 1.75 \cr
#' \eqn{R} \tab \code{R} \tab ideal gas constant \tab J / (mol K) \tab 8.3144598 \cr
#' \eqn{\sigma} \tab \code{s} \tab Stephan-Boltzmann constant \tab W / (m\eqn{^2} K\eqn{^4}) \tab 5.67e-08 \cr
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated}
#' }
#'
#' \bold{Baking (i.e. temperature response) parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{D_\mathrm{s,gmc}}{Ds_gmc} \tab \code{Ds_gmc} \tab empirical temperature response parameter \tab J / (mol K) \tab 487.29 \cr
#' \eqn{D_\mathrm{s,Jmax}}{Ds_Jmax} \tab \code{Ds_Jmax} \tab empirical temperature response parameter \tab J / (mol K) \tab 388.04 \cr
#' \eqn{E_\mathrm{a,\Gamma *}}{Ea_gammastar} \tab \code{Ea_gammastar} \tab empirical temperature response parameter \tab J / mol \tab 24459.97 \cr
#' \eqn{E_\mathrm{a,gmc}}{Ea_gmc} \tab \code{Ea_gmc} \tab empirical temperature response parameter \tab J / mol \tab 68901.56 \cr
#' \eqn{E_\mathrm{a,Jmax}}{Ea_Jmax} \tab \code{Ea_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 56095.18 \cr
#' \eqn{E_\mathrm{a,KC}}{Ea_KC} \tab \code{Ea_KC} \tab empirical temperature response parameter \tab J / mol \tab 80989.78 \cr
#' \eqn{E_\mathrm{a,KO}}{Ea_KO} \tab \code{Ea_KO} \tab empirical temperature response parameter \tab J / mol \tab 23719.97 \cr
#' \eqn{E_\mathrm{a,Rd}}{Ea_Rd} \tab \code{Ea_Rd} \tab empirical temperature response parameter \tab J / mol \tab 40446.75 \cr
#' \eqn{E_\mathrm{a,Vcmax}}{Ea_Vcmax} \tab \code{Ea_Vcmax} \tab empirical temperature response parameter \tab J / mol \tab 52245.78 \cr
#' \eqn{E_\mathrm{d,gmc}}{Ed_gmc} \tab \code{Ed_gmc} \tab empirical temperature response parameter \tab J / mol \tab 148788.56 \cr
#' \eqn{E_\mathrm{d,Jmax}}{Ed_Jmax} \tab \code{Ed_Jmax} \tab empirical temperature response parameter \tab J / mol \tab 121244.79
#' }
#'
#' \bold{Environment parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{C_\mathrm{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
#' \eqn{P} \tab \code{P} \tab atmospheric pressure \tab kPa \tab 101.3246 \cr
#' PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab umol quanta / (m^2 s) \tab 1500 \cr
#' \eqn{\mathrm{RH}}{RH} \tab \code{RH} \tab relative humidity \tab none \tab 0.50 \cr
#' \eqn{u} \tab \code{wind} \tab windspeed \tab m / s \tab 2
#' }
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{d} \tab \code{leafsize} \tab leaf characteristic dimension \tab m \tab 0.1 \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab chloroplastic CO2 compensation point (T_leaf) \tab Pa \tab \link[=bake]{calculated} \cr
#' \eqn{\Gamma*_{25}}{\Gamma_25*} \tab \code{gamma_star25} \tab chloroplastic CO2 compensation point (25 °C) \tab Pa \tab 3.743 \cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc} \tab mesophyll conductance to CO2 (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab \link[=bake]{calculated} \cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc25} \tab mesophyll conductance to CO2 (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{g_\mathrm{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{g_\mathrm{uc}}{g_uc} \tab \code{g_uc} \tab cuticular conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 0.1 \cr
#' \eqn{J_\mathrm{max25}}{J_max25} \tab \code{J_max25} \tab potential electron transport (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{k_\mathrm{mc}}{k_mc} \tab \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll \tab none \tab 1 \cr
#' \eqn{k_\mathrm{sc}}{k_sc} \tab \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface \tab none \tab 1 \cr
#' \eqn{k_\mathrm{uc}}{k_uc} \tab \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface \tab none \tab 1 \cr
#' \eqn{K_\mathrm{C25}}{K_C25} \tab \code{K_C25} \tab Michaelis constant for carboxylation (25 °C) \tab \eqn{\mu}mol / mol \tab 268.3 \cr
#' \eqn{K_\mathrm{C}}{K_C} \tab \code{K_C} \tab Michaelis constant for carboxylation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{K_\mathrm{O25}}{K_O25} \tab \code{K_O25} \tab Michaelis constant for oxygenation (25 °C) \tab \eqn{\mu}mol / mol \tab 165084.2\cr
#' \eqn{K_\mathrm{O}}{K_O} \tab \code{K_O} \tab Michaelis constant for oxygenation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
#' \eqn{\phi_J} \tab \code{phi_J} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
#' \eqn{R_\mathrm{d25}}{R_d25} \tab \code{R_d25} \tab nonphotorespiratory CO2 release (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2 \cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825 \cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} \tab \code{T_leaf} \tab leaf temperature \tab K \tab 298.15 \cr
#' \eqn{V_\mathrm{c,max25}}{V_c,max25} \tab \code{V_cmax25} \tab maximum rate of carboxylation (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 150 \cr
#' \eqn{V_\mathrm{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
#' \eqn{V_\mathrm{tpu25}}{V_tpu25} \tab \code{V_tpu25} \tab rate of triose phosphate utilization (25 °C) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 200 \cr
#' \eqn{V_\mathrm{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilisation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated}
#' }
#'
#' If \code{use_tealeaves = TRUE}, additional parameters are:
#'
#' \bold{Constants:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{c_p} \tab \code{c_p} \tab heat capacity of air \tab J / (g K) \tab 1.01 \cr
#' \eqn{R_\mathrm{air}}{R_air} \tab \code{R_air} \tab specific gas constant for dry air \tab J / (kg K) \tab 287.058\cr
#' }
#'
#' \bold{Environmental parameters:}
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{E_q} \tab \code{E_q} \tab energy per mole quanta \tab kJ / mol\eqn{^2} \tab 220 \cr
#' \eqn{f_\mathrm{PAR}}{f_PAR} \tab \code{f_par} \tab fraction of incoming shortwave radiation that is photosynthetically active radiation (PAR) \tab none \tab 0.5 \cr
#' \eqn{r} \tab \code{r} \tab reflectance for shortwave irradiance (albedo) \tab none \tab 0.2 \cr
#' \eqn{T_\mathrm{air}}{T_air} \tab \code{T_air} \tab air temperature \tab K \tab 298.15 \cr
#' }
#'
#' \bold{Leaf parameters:}
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{\alpha_\mathrm{l}}{\alpha_l} \tab \code{abs_l} \tab absorbtivity of longwave radiation (4 - 80 \eqn{\mu}m) \tab none \tab 0.97 \cr
#' \eqn{\alpha_\mathrm{s}}{\alpha_s} \tab \code{abs_s} \tab absorbtivity of shortwave radiation (0.3 - 4 \eqn{\mu}m) \tab none \tab 0.50 \cr
#' \eqn{g_\mathrm{sw}}{g_sw} \tab \code{g_sw} \tab stomatal conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab converted from \eqn{g_\mathrm{sc}}{g_sc} \cr
#' \eqn{g_\mathrm{uw}}{g_uw} \tab \code{g_uw} \tab cuticular conductance to H2O \tab (\eqn{\mu}mol H2O) / (m\eqn{^2} s Pa) \tab converted from \eqn{g_\mathrm{uc}}{g_uc} \cr
#' \eqn{\mathrm{logit}(sr)}{logit(sr)} \tab \code{logit_sr} \tab stomatal ratio (logit transformed) \tab none \tab converted from \eqn{k_\mathrm{sc}}{k_sc}
#' }
#'
#' @references
#'
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic
#' rate into contributions from different variables. Plant, Cell & Environment
#' 38: 1200-11.
#'
#' @examples
#' bake_par <- make_bakepar()
#' constants <- make_constants(use_tealeaves = FALSE)
#' enviro_par <- make_enviropar(use_tealeaves = FALSE)
#' leaf_par <- make_leafpar(use_tealeaves = FALSE)
#'
#' leaf_par <- make_leafpar(
#'   replace = list(
#'     g_sc = set_units(3, "umol/m^2/s/Pa"),
#'     V_cmax25 = set_units(100, "umol/m^2/s")
#'   ), use_tealeaves = FALSE
#' )
#' @export
make_leafpar <- function(replace = NULL, use_tealeaves) {

  # Defaults -----
  obj <- list(
    g_mc25 = set_units(4, umol / (m^2 * s * Pa)),
    g_sc = set_units(4, umol / (m^2 * s * Pa)),
    g_uc = set_units(0.1, umol / (m^2 * s * Pa)),
    gamma_star25 = set_units(3.743, Pa), # From Sharkey et al. 2007.
    # Newer source? Check bayCi
    J_max25 = set_units(200, umol / (m^2 * s)),
    k_mc = set_units(1),
    k_sc = set_units(1),
    k_uc = set_units(1),
    K_C25 = set_units(27.238, Pa), # From Sharkey et al. 2007.
    # Newer source? Check bayCi
    K_O25 = set_units(16.582, kPa), # From Sharkey et al. 2007.
    # Newer source? Check bayCi
    leafsize = set_units(0.1, m),
    phi_J = set_units(0.331),
    theta_J = set_units(0.825),
    R_d25 = set_units(2, umol / (m^2 * s)),
    T_leaf = set_units(298.15, K),
    V_cmax25 = set_units(150, umol / (m^2 * s)),
    V_tpu25 = set_units(200, umol / (m^2 * s))
  )

  if (use_tealeaves) {
    obj <- c(obj, list(
      abs_l = set_units(0.97),
      abs_s = set_units(0.5)
    ))
  }

  # Replace defaults ----
  if (!is.null(replace$T_leaf) & use_tealeaves) {
    warning("replace$T_leaf ignored when use_tealeaves is TRUE")
    replace$T_leaf <- NULL
  }

  par_equiv <- data.frame(
    tl = c("g_sw", "g_uw", "logit_sr"),
    ph = c("g_sc", "g_uc", "k_sc"),
    stringsAsFactors = FALSE
  )

  if (any(purrr::map_lgl(replace[par_equiv$tl], ~ !is.null(.x)))) {
    par_equiv %>%
      dplyr::filter(.data$tl %in% names(replace)) %>%
      dplyr::transmute(message = stringr::str_c(
        "\nIn `replace = list(...)`,
             tealeaves parameter ", .data$tl, " is not replacable. Use ",
        .data$ph, " instead."
      )) %>%
      dplyr::pull(.data$message) %>%
      stringr::str_c(collapse = "\n") %>%
      stop(call. = FALSE)
  }

  obj %<>% replace_defaults(replace)

  # Assign class and return ----
  obj %<>% photosynthesis::leaf_par(use_tealeaves)

  obj
}

#' make_enviropar
#' @rdname make_parameters
#' @export

make_enviropar <- function(replace = NULL, use_tealeaves) {

  # Defaults ----
  obj <- list(
    C_air = set_units(41, Pa),
    O = set_units(21.27565, kPa),
    P = set_units(101.3246, kPa),
    PPFD = set_units(1500, umol / m^2 / s),
    RH = set_units(0.5),
    wind = set_units(2, m / s)
  )

  # Add parameters for tealeaves ----
  if (use_tealeaves) {
    obj %<>% c(list(
      E_q = set_units(220, kJ / mol),
      f_par = set_units(0.5),
      r = set_units(0.2),
      T_air = set_units(298.15, K),
      T_sky = function(pars) {
        set_units(pars$T_air, K) - set_units(20, K) * set_units(pars$S_sw, W / m^2) / set_units(1000, W / m^2)
      }
    ))
  }

  # Replace defaults ----
  if ("T_sky" %in% names(replace)) {
    if (is.function(replace$T_sky)) {
      obj$T_sky <- replace$T_sky
      replace$T_sky <- NULL
    }
  }

  par_equiv <- data.frame(
    tl = c("S_sw"),
    ph = c("PPFD"),
    stringsAsFactors = FALSE
  )

  if (any(purrr::map_lgl(replace[par_equiv$tl], ~ !is.null(.x)))) {
    par_equiv %>%
      dplyr::filter(.data$tl %in% names(replace)) %>%
      dplyr::transmute(message = stringr::str_c(
        "\nIn `replace = list(...)`,
             tealeaves parameter ", .data$tl,
        " is not replacable. Use ", .data$ph, " instead."
      )) %>%
      dplyr::pull(.data$message) %>%
      stringr::str_c(collapse = "\n") %>%
      stop(call. = FALSE)
  }

  obj %<>% replace_defaults(replace)

  # Assign class and return ----
  obj %<>% photosynthesis::enviro_par(use_tealeaves)

  obj
}

#' make_bakepar
#' @rdname make_parameters
#' @export

make_bakepar <- function(replace = NULL) {

  # Defaults -----
  obj <- list(
    Ds_gmc = set_units(487.29, J / mol / K),
    Ds_Jmax = set_units(388.04, J / mol / K),
    Ea_gammastar = set_units(24459.97, J / mol),
    Ea_gmc = set_units(68901.56, J / mol),
    Ea_Jmax = set_units(56095.18, J / mol),
    Ea_KC = set_units(80989.78, J / mol),
    Ea_KO = set_units(23719.97, J / mol),
    Ea_Rd = set_units(40446.75, J / mol),
    Ea_Vcmax = set_units(52245.78, J / mol),
    Ea_Vtpu = set_units(52245.78, J / mol),
    Ed_gmc = set_units(148788.56, J / mol),
    Ed_Jmax = set_units(121244.79, J / mol)
  )

  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% photosynthesis::bake_par()

  obj
}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants <- function(replace = NULL, use_tealeaves) {

  # Defaults -----
  obj <- list(
    D_c0 = set_units(1.29e-5, m^2 / s),
    D_h0 = set_units(1.90e-5, m^2 / s),
    D_m0 = set_units(1.33e-5, m^2 / s),
    D_w0 = set_units(2.12e-5, m^2 / s),
    epsilon = set_units(0.622),
    eT = set_units(1.75),
    G = set_units(9.8, m / s^2),
    nu_constant = function(Re, type, T_air, T_leaf, surface, unitless) {
      if (!unitless) {
        stopifnot(units(T_air)$numerator == "K" &
          length(units(T_air)$denominator) == 0L)
        stopifnot(units(T_leaf)$numerator == "K" &
          length(units(T_leaf)$denominator) == 0L)
      }

      type %<>% match.arg(c("free", "forced"))

      if (identical(type, "forced")) {
        if (unitless) {
          if (Re <= 4000) ret <- list(a = 0.6, b = 0.5)
          if (Re > 4000) ret <- list(a = 0.032, b = 0.8)
        } else {
          if (Re <= set_units(4000)) ret <- list(a = 0.6, b = 0.5)
          if (Re > set_units(4000)) ret <- list(a = 0.032, b = 0.8)
        }
        return(ret)
      }

      if (identical(type, "free")) {
        surface %<>% match.arg(c("lower", "upper"))
        if ((surface == "upper" & T_leaf > T_air) |
          (surface == "lower" & T_leaf < T_air)) {
          ret <- list(a = 0.5, b = 0.25)
        } else {
          ret <- list(a = 0.23, b = 0.25)
        }
        return(ret)
      }
    },
    R = set_units(8.3144598, J / (mol * K)),
    s = set_units(5.67e-08, W / (m^2 * K^4)),
    sh_constant = function(type, unitless) {
      type %>%
        match.arg(c("free", "forced")) %>%
        switch(forced = 0.33, free = 0.25)
    }
  )

  # Replace defaults -----
  if ("nu_constant" %in% names(replace)) {
    stopifnot(is.function(replace$nu_constant))
    obj$nu_constant <- replace$nu_constant
    replace$nu_constant <- NULL
  }

  if ("sh_constant" %in% names(replace)) {
    stopifnot(is.function(replace$sh_constant))
    obj$sh_constant <- replace$sh_constant
    replace$sh_constant <- NULL
  }

  # Add parameters for tealeaves ----
  if (use_tealeaves) {
    obj %<>% c(list(
      c_p = set_units(1.01, J / g / K),
      R_air = set_units(287.058, J / kg / K)
    ))
  }

  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% photosynthesis::constants(use_tealeaves)

  obj
}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#' @noRd

replace_defaults <- function(obj, replace) {
  if (!is.null(replace)) {
    stopifnot(is.list(replace))
    stopifnot(all(sapply(replace, inherits, what = "units")))
    stopifnot(all(sapply(replace, is.numeric)))
    x <- names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not
                      recognized:\n%s", paste0(x[!x %in% names(obj)],
        collapse = "\n"
      )))
      x %<>% .[. %in% names(obj)]
    }
    obj[x] <- replace[x]
  }

  obj
}
