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
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
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
#' \code{phi} \tab initial slope of the response of J to PPFD (unitless) \cr
#' \code{PPFD} \tab photosynthetic photon flux density (umol quanta / (m\eqn{^2} s)) \cr
#' \code{R_d25} \tab nonphotorespiratory CO2 release  at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{RH} \tab relative humidity (unitless) \cr
#' \code{theta_J} \tab curvature factor for light-response curve (unitless) \cr
#' \code{T_air} \tab air temperature (K) \cr
#' \code{T_leaf} \tab leaf tempearture (K) \cr
#' \code{V_cmax25} \tab maximum rate of carboxylation at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
#' \code{V_tpu25} \tab rate of triose phosphate utilisation at 25 °C (\eqn{\mu}mol CO2 / (m\eqn{^2} s)) \cr
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
#' \code{photosynthesis}: This function uses \code{photo} to simulate photosynthesis over multiple parameter sets that are generated using \code{\link[tidyr]{crossing}}. \cr
#' 
#' @examples 
#' # Single parameter set with 'photo'
#' 
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' bake_par <- make_bakepar()
#' constants <- make_constants()
#' photo(leaf_par, enviro_par, bake_par, constants)
#' 
#' # Multiple parameter sets with 'photosynthesis'
#' 
#' leaf_par <- make_leafpar(
#'   replace = list(
#'     T_leaf = set_units(c(293.14, 298.15), "K")
#'     )
#'   )
#' enviro_par <- make_enviropar()
#' bake_par <- make_bakepar()
#' constants <- make_constants()
#' photosynthesis(leaf_par, enviro_par, bake_par, constants)
#' 
#' @encoding UTF-8
#' 
#' @export
#' 

photosynthesis <- function(leaf_par, enviro_par, bake_par, constants, 
                           progress = TRUE, quiet = FALSE) {
  
  # Check inputs ----
  leaf_par %<>% leaf_par()
  enviro_par %<>% enviro_par()
  bake_par %<>% bake_par()
  constants %<>% constants()
  
  # Capture units ----
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  # Make parameter sets ----
  pars %<>%
    names() %>%
    glue::glue("{x} = pars${x}", x = .) %>%
    stringr::str_c(collapse = ", ") %>%
    glue::glue("tidyr::crossing({x})", x = .) %>%
    parse(text = .) %>%
    eval() %>%
    purrr::transpose()
  
  tidyr::crossing(i = seq_len(length(pars)),
                  par = names(pars[[1]])) %>%
    dplyr::transmute(ex = glue::glue("units(pars[[{i}]]${par}) <<- par_units${par}", 
                                     i = .data$i, par = .data$par)) %>%
    dplyr::pull("ex") %>%
    parse(text = .) %>%
    eval()
  
  # Simulate ----
  if (!quiet) {
    glue::glue("\nSolving for photosynthetic rate from {n} parameter set{s} ...", 
               n = length(pars), s = dplyr::if_else(length(pars) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (progress) pb <- dplyr::progress_estimated(length(pars))
  
  soln <- suppressWarnings(pars %>%
    purrr::map_dfr(~{
      
      ret <- photo(leaf_par(.x), enviro_par(.x), bake_par, constants, quiet = TRUE)
      if (progress) pb$tick()$print()
      ret
      
    }))
  
  # Reassign units ----
  colnames(soln) %>%
    glue::glue("units(soln${x}) <<- par_units${x}", x = .) %>%
    parse(text = .) %>%
    eval()

  soln %>%
    dplyr::select(tidyselect::ends_with("25")) %>%
    colnames() %>%
    stringr::str_remove("25$") %>%
    glue::glue("units(soln${x}) <<- par_units${x}25", x = .) %>%
    parse(text = .) %>%
    eval()

  units(soln$C_chl) <- "Pa"
  units(soln$g_tc) <- "umol/m^2/s/Pa"
  units(soln$A) <- "umol/m^2/s"

  # Return ----
  soln
  
}

#' Simulate C3 photosynthesis
#' @description \code{photo}: simulate C3 photosynthesis over a single parameter set
#' @rdname photosynthesis
#' @export

photo <- function(leaf_par, enviro_par, bake_par, constants, quiet = FALSE) {
  
  # Check inputs and bake ----
  leaf_par %<>% bake(bake_par, constants)
  enviro_par %<>% enviro_par()
  
  pars <- c(leaf_par, enviro_par, constants)
  
  # Find intersection between photosynthetic supply and demand curves -----
  .f <- function(C_chl, pars) {
    
    C_chl %<>% set_units("Pa")
    (A_supply(C_chl, pars) - A_demand(C_chl, pars)) %>%
      set_units("umol/m^2/s") %>%
      drop_units()
    
  }
  
  if (!quiet) {
    "\nSolving for C_chl ..." %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  fit <- tryCatch({

    lower <- 0.1
    upper <- drop_units(set_units(max(c(set_units(10, "Pa"), pars$C_air)), "Pa"))
    stats::uniroot(.f, pars = pars, lower = lower, upper = upper, check.conv = TRUE)

  }, finally = {
    fit <- list(root = NA, f.root = NA, convergence = 1)
  })
  
  soln <- data.frame(C_chl = set_units(fit$root, "Pa"), value = fit$f.root, 
                     convergence = dplyr::if_else(is.null(fit$convergence), 0, 1))
  
  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }
  
  # Check results -----
  if (soln$convergence == 1) {
    "stats::uniroot did not converge, NA returned. Inspect parameters carefully." %>%
      crayon::red() %>%
      message()
  }
  
  stopifnot(drop_units(A_supply(soln$C_chl, pars) - 
                         A_demand(soln$C_chl, pars)) == soln$value)
  
  # Return -----
  soln %<>% 
    dplyr::bind_cols(as.data.frame(leaf_par)) %>%
    dplyr::bind_cols(as.data.frame(enviro_par))

  soln$g_tc <- set_units(.get_gtc(pars), "umol/m^2/s/Pa")
  soln$A <- set_units(A_supply(soln$C_chl, pars), "umol/m^2/s")
  soln  
  
}

#' CO2 supply and demand function (mol / m^2 s)
#' 
#' @param C_chl Chloroplastic CO2 concentration in Pa of class \code{units}
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
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
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' bake_par <- make_bakepar()
#' constants <- make_constants()
#' leaf_par <- bake(leaf_par, bake_par, constants)
#' # Or bake with piping (need library(magrittr))
#' # leaf_par %<>% bake(bake_par, constants)
#' 
#' pars <- c(leaf_par, enviro_par, constants)
#' C_chl <- set_units(35, "Pa")
#' 
#' A_supply(C_chl, pars)
#' 
#' A_demand(C_chl, pars)
#' 
#' @export

A_supply <- function(C_chl, pars) {
  
  g_tc <- .get_gtc(pars)
  As <- set_units(g_tc * (pars$C_air - C_chl), "umol/m^2/s")
  As
  
}

#' A_demand
#' @rdname A_supply
#' @export

A_demand <- function(C_chl, pars) {
  
  set_units((set_units(1) - pars$gamma_star / C_chl) * FvCB(C_chl, pars)$A - 
              pars$R_d, "umol/m^2/s")
  
}

