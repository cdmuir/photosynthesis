#' Simulate C3 photosynthesis
#' 
#' \code{photosynthesis}: simulate C3 photosynthesis over multiple parameter sets
#' 
#' @param leaf_par A list of leaf parameters inheriting class \code{leaf_par}. This can be generated using the \code{make_leafpar} function.
#' 
#' @param enviro_par A list of environmental parameters inheriting class \code{enviro_par}. This can be generated using the \code{make_enviropar} function.
#' 
#' @param bake_par A list of temperature response parameters inheriting class \code{bake_par}. This can be generated using the \code{make_bakepar} function.
#' 
#' @param constants A list of physical constants inheriting class \code{constants}. This can be generated using the \code{make_constants} function.
#' 
#' @param use_tealeaves Logical. Should leaf energy balance be used to calculate leaf temperature (T_leaf)? If TRUE, \code{\link[tealeaves:tleaves]{tleaf}} calculates T_leaf. If FALSE, user-defined T_leaf is used. Additional parameters and constants are required, see \code{\link{make_parameters}}.
#' 
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
#' 
#' @param assert_units Logical. Should parameter \code{units} be checked? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.
#' 
#' @param parallel Logical. Should parallel processing be used via \code{\link[furrr]{future_map}}?
#' 
#' @return 
#' A data.frame with the following \code{units} columns \cr
#' 
#' \tabular{ll}{
#' 
#' \bold{Input:} \tab \cr
#' \cr
#' \code{C_air} \tab atmospheric CO2 concentration (Pa) \cr
#' \code{g_mc25} \tab mesophyll conductance to CO2 at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{g_sc} \tab stomatal conductance to CO2 (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{g_uc} \tab cuticular conductance to CO2 (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{gamma_star25} \tab chloroplastic CO2 compensation point at 25 °C (Pa) \cr
#' \code{J_max25} \tab potential electron transport at 25 °C (\eqn{\mu}mol CO2) / (m\eqn{^2} s) \cr
#' \code{K_C25} \tab Michaelis constant for carboxylation at 25 °C (\eqn{\mu}mol / mol) \cr
#' \code{K_O25} \tab Michaelis constant for oxygenation at 25 °C (\eqn{\mu}mol / mol) \cr
#' \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll (unitless) \cr
#' \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface (unitless) \cr
#' \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface (unitless) \cr
#' \code{leafsize} \tab leaf characteristic dimension (m) \cr
#' \code{O} \tab atmospheric O2 concentration (kPa) \cr
#' \code{P} \tab atmospheric pressure (kPa) \cr
#' \code{phi_J} \tab initial slope of the response of J to PPFD (unitless) \cr
#' \code{PPFD} \tab photosynthetic photon flux density (umol quanta / (m\eqn{^2} s)) \cr
#' \code{R_d25} \tab nonphotorespiratory CO2 release  at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{RH} \tab relative humidity (unitless) \cr
#' \code{theta_J} \tab curvature factor for light-response curve (unitless) \cr
#' \code{T_air} \tab air temperature (K) \cr
#' \code{T_leaf} \tab leaf temperature (K) \cr
#' \code{V_cmax25} \tab maximum rate of carboxylation at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_tpu25} \tab rate of triose phosphate utilization at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{wind} \tab wind speed (m / s) \cr
#' \cr
#' \bold{Baked Input:} \tab \cr
#' \cr
#' \code{g_mc} \tab mesophyll conductance to CO2 at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{gamma_star} \tab chloroplastic CO2 compensation point at \code{T_leaf} (Pa) \cr
#' \code{J_max} \tab potential electron transport at \code{T_leaf} (\eqn{\mu}mol CO2) / (m\eqn{^2} s) \cr
#' \code{K_C} \tab Michaelis constant for carboxylation at \code{T_leaf} (\eqn{\mu}mol / mol) \cr
#' \code{K_O} \tab Michaelis constant for oxygenation at \code{T_leaf}(\eqn{\mu}mol / mol) \cr
#' \code{R_d} \tab nonphotorespiratory CO2 release  at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_cmax} \tab maximum rate of carboxylation at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_tpu} \tab rate of triose phosphate utilisation at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \cr
#' \bold{Output:} \tab \cr
#' \cr
#' \code{A} \tab photosynthetic rate at \code{C_chl} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{C_chl} \tab chloroplastic CO2 concentration where \code{A_supply} intersects \code{A_demand} (Pa) \cr
#' \code{g_tc} \tab total conductance to CO2 at \code{T_leaf} (\eqn{\mu}mol CO2 / (m\eqn{^2} s Pa)) \cr
#' \code{value} \tab \code{A_supply} - \code{A_demand} (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) at \code{C_chl} \cr
#' \code{convergence} \tab convergence code (0 = converged)
#' }
#' 
#' @details 
#' 
#' \code{photo}: This function takes simulates photosynthetic rate using the Farquhar-von Caemmerer-Berry (\code{\link{FvCB}}) model of C3 photosynthesis for single combined set of leaf parameters (\code{\link{leaf_par}}), environmental parameters (\code{\link{enviro_par}}), and physical constants (\code{\link{constants}}). Leaf parameters are provided at reference temperature (25 °C) and then "baked" to the appropriate leaf temperature using temperature response functions (see \code{\link{bake}}). \cr
#' \cr
#' \code{photosynthesis}: This function uses \code{photo} to simulate photosynthesis over multiple parameter sets that are generated using \code{\link[purrr:cross]{cross_df}}. \cr
#' 
#' @examples 
#' # Single parameter set with 'photo'
#' 
#' bake_par <- make_bakepar()
#' constants <- make_constants(use_tealeaves = FALSE)
#' enviro_par <- make_enviropar(use_tealeaves = FALSE)
#' leaf_par <- make_leafpar(use_tealeaves = FALSE)
#' photo(leaf_par, enviro_par, bake_par, constants,
#'       use_tealeaves = FALSE)
#' 
#' # Multiple parameter sets with 'photosynthesis'
#' 
#' leaf_par <- make_leafpar(
#'   replace = list(
#'     T_leaf = set_units(c(293.14, 298.15), "K")
#'     ), use_tealeaves = FALSE
#'   )
#' photosynthesis(leaf_par, enviro_par, bake_par, constants,
#'                use_tealeaves = FALSE)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

photosynthesis <- function(leaf_par, enviro_par, bake_par, constants, 
                           use_tealeaves, progress = TRUE, quiet = FALSE,
                           assert_units = TRUE, parallel = FALSE) {
  
  T_air <- NULL
  if (!use_tealeaves & !is.null(enviro_par$T_air)) {
    if (!quiet) {
      message(glue::glue("Both air and leaf temperature are provided and fixed: T_air = {T_air}; T_leaf = {T_leaf}", T_air = enviro_par$T_air,
                         T_leaf = leaf_par$T_leaf))
    }
    T_air <- enviro_par$T_air
  }
  
  # Assert units ----
  if (assert_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% photosynthesis::constants(use_tealeaves)
    enviro_par %<>% photosynthesis::enviro_par(use_tealeaves)
    leaf_par %<>% photosynthesis::leaf_par(use_tealeaves)
    if (!is.null(T_air)) enviro_par$T_air <- set_units(T_air, K)
  }

  pars <- c(leaf_par, enviro_par)
  
  tsky_function <- NULL
  if (is.function(pars$T_sky)) {
    tsky_function <- pars$T_sky
    pars$T_sky <- NULL
  }

  # Capture units ----
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  if (!is.null(T_air)) {
    par_units$T_air <- units(enviro_par$T_air) 
  } else {
    if (!use_tealeaves) par_units$T_air <- units(leaf_par$T_leaf) 
  }
  
  # Make parameter sets ----
  ## cross_df() removes units. 
  ## This code will cause errors if units are not properly set
  pars %<>% purrr::cross_df()
  
  # Calculate T_leaf using energy balance or set to T_air ----
  if (use_tealeaves) {
    
    # This is an inefficient hack
    E_q <- pars$E_q
    PPFD <- pars$PPFD
    f_par <- pars$f_par
    g_sc <- pars$g_sc
    g_uc <- pars$g_uc
    
    units(E_q) <- par_units$E_q
    units(PPFD) <- par_units$PPFD
    units(f_par) <- par_units$f_par
    units(g_sc) <- par_units$g_sc
    units(g_uc) <- par_units$g_uc
    
    pars$S_sw <- set_units(E_q * PPFD / f_par, W / m ^ 2)
    pars$g_sw <- set_units(constants$D_w0 / constants$D_c0 * g_sc, 
                           umol/m^2/Pa/s)
    pars$g_uw <- set_units(constants$D_w0 / constants$D_c0 * g_uc,
                           umol/m^2/Pa/s)
    pars$logit_sr <- stats::qlogis(pars$k_sc / (1 + pars$k_sc))

    par_units$S_sw <- units(units::make_units(W/m^2))
    par_units$g_sw <- par_units$g_sc
    par_units$g_uw <- par_units$g_uc
    par_units$logit_sr <- par_units$k_sc

    pars$S_sw %<>% drop_units()
    pars$g_sw %<>% drop_units()
    pars$g_uw %<>% drop_units()

    tlp <- pars %>%
      as.list() %>%
      purrr::map(unique) %>%
      tealeaves::leaf_par()
     
    tep <- pars %>%
      as.list() %>%
      purrr::map(unique) 
    
    if (!is.null(tsky_function)) {
      tep$T_sky <- tsky_function
    }
    
    tep %<>% tealeaves::enviro_par()

    tcs <- tealeaves::constants(constants)
    
    tl <- tealeaves::tleaves(tlp, tep, tcs, progress = FALSE, quiet = TRUE, 
                             set_units = FALSE, parallel = parallel)
    
    par_units$T_leaf <- units(tl$T_leaf)
    
    tl <- tl %>% dplyr::rename(tealeaves_convergence = .data$convergence, 
                               tealeaves_value = .data$value)
    
    ## Drop units and join
    suppressMessages(
      pars %<>% dplyr::full_join(dplyr::mutate_if(tl, ~ is(.x, "units"),
                                                  drop_units))
    )
    
  } else {
    
    if (is.null(T_air)) pars$T_air <- pars$T_leaf

  }
  
  # Simulate ----
  soln <- find_As(pars, bake_par, constants, par_units, progress, quiet,
                  parallel)
  
  # Return ----
  soln
  
}

find_As <- function(par_sets, bake_par, constants, par_units, progress, quiet,
                    parallel) {
  
  if (!quiet) {
    glue::glue("\nSolving for photosynthetic rate from {n} parameter set{s} ...", 
               n = nrow(par_sets), s = dplyr::if_else(length(par_sets) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (parallel) future::plan("multisession")
  
  if (progress & !parallel) pb <- dplyr::progress_estimated(nrow(par_sets))
  
  soln <- suppressWarnings(
    par_sets %>%
      as.list() %>%
      purrr::transpose() %>%
      furrr::future_map_dfr(~{
        
        ret <- photosynthesis::photo(
          leaf_par = .x, enviro_par = .x, bake_par = bake_par,
          constants = constants, use_tealeaves = FALSE, quiet = TRUE, 
          assert_units = FALSE, check = FALSE, prepare_for_tleaf = FALSE
        )
        if (progress & !parallel) pb$tick()$print()
        ret
        
      }, .progress = progress) %>%
      dplyr::select_at(
        dplyr::vars(-tidyselect::one_of(stringr::str_c(colnames(.), "1")))
      )
  )
  
  # Reassign units ----
  soln_env <- environment()
  colnames(soln) %>%
    glue::glue("units(soln${x}) <- par_units${x}", x = .) %>%
    parse(text = .) %>%
    eval(envir = soln_env)
  
  soln %>%
    dplyr::select(tidyselect::ends_with("25")) %>%
    colnames() %>%
    stringr::str_remove("25$") %>%
    glue::glue("units(soln${x}) <- par_units${x}25", x = .) %>%
    parse(text = .) %>%
    eval(envir = soln_env)
  
  soln$C_chl %<>% set_units(Pa)
  soln$g_tc %<>% set_units(umol/m^2/s/Pa)
  soln$A %<>% set_units(umol/m^2/s)
  
  soln
  
}

#' Simulate C3 photosynthesis
#' @description \code{photo}: simulate C3 photosynthesis over a single parameter set
#' @rdname photosynthesis
#' 
#' @param check Logical. Should arguments checks be done? This is intended to be disabled when \code{\link{photo}} is called from \code{\link{photosynthesis}} Default is TRUE.
#'
#' @param prepare_for_tleaf Logical. Should arguments additional calculations for \code{\link[tealeaves:tleaves]{tleaf}}? This is intended to be disabled when \code{\link{photo}} is called from \code{\link{photosynthesis}}. Default is \code{use_tealeaves}.
#' 
#' @export

photo <- function(leaf_par, enviro_par, bake_par, constants, 
                  use_tealeaves, quiet = FALSE, assert_units = TRUE,
                  check = TRUE, prepare_for_tleaf = use_tealeaves) {
  
  checkmate::assert_flag(check)
  checkmate::assert_flag(prepare_for_tleaf)
  
  if (check) {
    checkmate::assert_class(bake_par, "bake_par")
    checkmate::assert_class(constants, "constants")
    checkmate::assert_class(enviro_par, "enviro_par")
    checkmate::assert_class(leaf_par, "leaf_par")
    checkmate::assert_flag(use_tealeaves)
    checkmate::assert_flag(quiet)
    checkmate::assert_flag(assert_units)
  }
  
  T_air <- NULL
  if (!use_tealeaves & !is.null(enviro_par$T_air)) {
    if (!quiet) {
      message(glue::glue("Both air and leaf temperature are provided and fixed: T_air = {T_air}; T_leaf = {T_leaf}", T_air = enviro_par$T_air,
                       T_leaf = leaf_par$T_leaf))
    }
    T_air <- enviro_par$T_air
  }
  
  # Set units and bake ----
  if (assert_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% photosynthesis::constants(use_tealeaves)
    enviro_par %<>% photosynthesis::enviro_par(use_tealeaves)
    leaf_par %<>% photosynthesis::leaf_par(use_tealeaves)
    if (!is.null(T_air)) enviro_par$T_air <- set_units(T_air, K)
  }

  # Calculate T_leaf using energy balance ----
  if (use_tealeaves) {

    if (prepare_for_tleaf) {
      enviro_par$S_sw <- set_units(enviro_par$E_q * enviro_par$PPFD / 
                                     enviro_par$f_par, W/m^2)
      leaf_par$g_sw <- set_units(constants$D_w0 / constants$D_c0 * leaf_par$g_sc,
                                 umol/m^2/Pa/s)
      leaf_par$g_uw <- set_units(constants$D_w0 / constants$D_c0 * leaf_par$g_uc,
                                 umol/m^2/Pa/s)
      leaf_par$logit_sr <- stats::qlogis(leaf_par$k_sc / (set_units(1) + 
                                                            leaf_par$k_sc))
    }
    
    tl <- tealeaves::tleaf(leaf_par = leaf_par, enviro_par = enviro_par, 
                           constants = constants, quiet = TRUE, 
                           set_units = TRUE)
    tl <- tl %>% dplyr::rename(tealeaves_convergence = .data$convergence, 
                               tealeaves_value = .data$value)
    leaf_par$T_leaf <- tl$T_leaf
    
  }
  
  leaf_par %<>% bake(bake_par, constants, assert_units = FALSE)
  
  pars <- c(leaf_par, enviro_par, constants) %>%
    purrr::map_if(~ inherits(.x, "units"), drop_units)
  if (!use_tealeaves & is.null(pars$T_air)) pars$T_air <- pars$T_leaf
  
  # Find intersection between photosynthetic supply and demand curves -----
  soln <- find_A(pars, quiet)
  
  # Check results -----
  if (soln$convergence == 1) {
    "stats::uniroot did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
  # Return -----
  # This is a hack needed for `photosynthesis()` because leaf_par and enviro_par
  # are both passed from pars = c(leaf_par, enviro_par), so they have redundant
  # information. This checks that everything is identical, then gets rid of 
  # redundant parameters.
  shared_pars <- intersect(names(leaf_par), names(enviro_par))
  checkmate::assert_true(all(unlist(leaf_par[shared_pars]) == 
                               unlist(enviro_par[shared_pars])))
  leaf_par[shared_pars] <- NULL
  
  soln %<>% 
    dplyr::bind_cols(as.data.frame(leaf_par)) %>%
    dplyr::bind_cols(as.data.frame(enviro_par))

  soln$C_chl %<>% set_units(Pa)
  soln$g_tc %<>% set_units(umol/m^2/s/Pa)
  soln$A %<>% set_units(umol/m^2/s)

  soln  
  
}

find_A <- function(unitless_pars, quiet) {
  
  .f <- function(C_chl, unitless_pars) {
    A_supply(C_chl, unitless_pars, unitless = TRUE) - 
      A_demand(C_chl, unitless_pars, unitless = TRUE)
  }
  
  if (!quiet) {
    "\nSolving for C_chl ..." %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  fit <- tryCatch({
    stats::uniroot(.f, unitless_pars = unitless_pars, lower = 0.1, 
                   upper = max(c(10, unitless_pars$C_air)), check.conv = TRUE)
  }, finally = {
    fit <- list(root = NA, f.root = NA, convergence = 1)
  })
  
  soln <- data.frame(C_chl = fit$root, value = fit$f.root, 
                     convergence = dplyr::if_else(is.null(fit$convergence), 0, 1))
  
  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }
  
  soln$g_tc <- .get_gtc(unitless_pars, unitless = TRUE)
  soln$A <- A_supply(soln$C_chl, unitless_pars, unitless = TRUE)

  soln
  
}

#' CO2 supply and demand function (mol / m^2 s)
#' 
#' This function is not intended to be called by users directly.
#' 
#' @param C_chl Chloroplastic CO2 concentration in Pa of class \code{units}
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
#' @param unitless Logical. Should \code{units} be set? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.
#' 
#' @return Value in mol / (m^2 s) of class \code{units}
#' 
#' @details 
#' 
#' \bold{Supply function:}
#' \cr
#' \deqn{A = g_\mathrm{tc} (C_\mathrm{air} - C_\mathrm{chl})}{A = g_tc (C_air - C_chl)}
#' 
#' \bold{Demand function:}
#' \cr
#' \deqn{A = (1 - \Gamma* / C_\mathrm{chl}) \mathrm{min}(W_\mathrm{carbox}, W_\mathrm{regen}, W_\mathrm{tpu}) - R_\mathrm{d}}{A = (1 - \Gamma* / C_chl) min(W_carbox, W_regen, W_tpu) - R_d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{A} \tab \code{A} \tab photosynthetic rate \tab \eqn{\mu}mol CO2 / (m^2 s) \tab calculated \cr
#' \eqn{g_\mathrm{tc}}{g_tc} \tab \code{g_tc} \tab total conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab \link[=.get_gtc]{calculated} \cr
#' \eqn{C_\mathrm{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{C_\mathrm{chl}}{C_chl} \tab \code{C_chl} \tab chloroplastic CO2 concentration \tab Pa \tab calculated\cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2 \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab chloroplastic CO2 compensation point \tab Pa \tab 3.743
#' }
#' 
#' @examples 
#' bake_par <- make_bakepar()
#' constants <- make_constants(use_tealeaves = FALSE)
#' enviro_par <- make_enviropar(use_tealeaves = FALSE)
#' leaf_par <- make_leafpar(use_tealeaves = FALSE)
#' leaf_par <- bake(leaf_par, bake_par, constants)
#' # Or bake with piping (need library(magrittr))
#' # leaf_par %<>% bake(bake_par, constants)
#' enviro_par$T_air <- leaf_par$T_leaf
#' 
#' pars <- c(leaf_par, enviro_par, constants)
#' C_chl <- set_units(35, "Pa")
#' 
#' A_supply(C_chl, pars)
#' 
#' A_demand(C_chl, pars)
#' 
#' @export

A_supply <- function(C_chl, pars, unitless = FALSE) {
  
  g_tc <- .get_gtc(pars, unitless)
  
  if (unitless) {
    As <- g_tc * (pars$C_air - C_chl)
  } else {
    As <- set_units(g_tc * (pars$C_air - C_chl), umol/m^2/s)
  }
  As
  
}

#' A_demand
#' @rdname A_supply
#' @export

A_demand <- function(C_chl, pars, unitless = FALSE) {
  
  if (unitless) {
    Ad <- (1 - pars$gamma_star / C_chl) * FvCB(C_chl, pars, unitless)$A - pars$R_d
  } else {
    Ad <- set_units((set_units(1) - pars$gamma_star / C_chl) * 
                      FvCB(C_chl, pars, unitless)$A - pars$R_d, umol/m^2/s)
  }
  
  Ad
  
}
