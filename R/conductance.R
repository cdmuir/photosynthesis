#' Conductance to CO2 (umol / (m^2 s Pa))
#'
#' @inheritParams A_supply
#'
#' @name CO2_conductance
#' 
#' @details 
#' 
#' Total conductance to CO2 is the sum of parallel conductances on the lower (\eqn{g_\mathrm{c,lower}}{gc_lower}) and upper (\eqn{g_\mathrm{c,upper}}{gc_upper}) leaf portions:
#' 
#'  \deqn{g_\mathrm{c,total} = g_\mathrm{c,lower} + g_\mathrm{c,upper}}{gc_total = gc_lower + gc_upper}
#'  
#' Each partial conductance consists of two parallel conductances, the cuticular conductance (\eqn{g_\mathrm{u,c}}{g_uc}) and the in-series conductances through mesophyll (\eqn{g_\mathrm{m,c}}{g_mc}), stomata (\eqn{g_\mathrm{s,c}}{g_sc}), and boundary layer (\eqn{g_\mathrm{b,c}}{g_bc}). To simplify the formula, I use substitute resistance where \eqn{r_x = 1 / g_x}. For surface \eqn{i}:
#' 
#' \deqn{g_{\mathrm{c},i} = g_{\mathrm{u},i} + (1 / (r_{\mathrm{m},i} + r_{\mathrm{s},i} + r_{\mathrm{b},i}))}{g_ci = g_ui + (1 / (r_mi + r_si + r_bi))}
#' 
#' The cuticular, stomatal, and mesophyll conductances can be the same or different for upper and lower. The partitioning factors (\eqn{k_x}) divide the conductance between surfaces while keeping the total conductance constant:
#' 
#' \deqn{g_{x,\mathrm{lower}} = g_x (1 / (1 + k_x))}{gx_lower = g_x (1 / (1 + k_x))}
#' \deqn{g_{x,\mathrm{upper}} = g_x (k_x / (1 + k_x))}{gx_upper = g_x (k_x / (1 + k_x))}
#' \deqn{g_x = g_{x,\mathrm{lower}} + g_{x,\mathrm{upper}}}{g_x = gx_lower + gx_upper}
#' 
#' How the partitioning factors work: \cr
#' \tabular{ll}{
#' \eqn{k_x} \tab description \cr
#' 0 \tab all conductance on \bold{lower} surface/portion \cr
#' 0.5 \tab 2/3 conductance on \bold{lower} surface \cr
#' 1 \tab conductance evenly divided between surfaces/portions \cr
#' 2 \tab 2/3 conductance on \bold{upper} surface \cr
#' Inf \tab all conductance on \bold{upper} surface/portion
#' }
#' 
#' The boundary layer conductances for each are calculated on the basis of mass and heat transfer (see \code{\link{.get_gbc}}).
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc} \tab mesophyll conductance to CO2 (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab \link[=bake]{calculated} \cr
#' \eqn{g_\mathrm{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 4 \cr
#' \eqn{g_\mathrm{uc}}{g_uc} \tab \code{g_uc} \tab cuticular conductance to CO2 \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s Pa) \tab 0.1 \cr
#' \eqn{k_\mathrm{mc}}{k_mc} \tab \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll \tab none \tab 1 \cr
#' \eqn{k_\mathrm{sc}}{k_sc} \tab \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface \tab none \tab 1 \cr
#' \eqn{k_\mathrm{uc}}{k_uc} \tab \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface \tab none \tab 1 \cr
#' }
#' 
#' @encoding UTF-8
#' 

NULL

#'  - g_tc: total conductance to CO2
#' 
#' @rdname CO2_conductance

.get_gtc <- function(pars, unitless) {
  
  gbc_lower <- .get_gbc(pars, "lower", unitless)
  gmc_lower <- .get_gmc(pars, "lower", unitless)
  gsc_lower <- .get_gsc(pars, "lower", unitless)
  guc_lower <- .get_guc(pars, "lower", unitless)
  
  gbc_upper <- .get_gbc(pars, "upper", unitless)
  gmc_upper <- .get_gmc(pars, "upper", unitless)
  gsc_upper <- .get_gsc(pars, "upper", unitless)
  guc_upper <- .get_guc(pars, "upper", unitless)
  
  rc_lower <- 1 / gmc_lower + 1 / gsc_lower + 1 / gbc_lower
  gc_lower <- 1 / rc_lower
  gc_lower %<>% magrittr::add(guc_lower)

  rc_upper <- 1 / gmc_upper + 1 / gsc_upper + 1 / gbc_upper
  gc_upper <- 1 / rc_upper
  gc_upper %<>% magrittr::add(guc_upper)
  
  g_tc <- gc_lower + gc_upper
  
  if (!unitless) g_tc %<>% set_units(umol/m^2/s/Pa)
  
  g_tc
  
}

#'  - g_uc: cuticular conductance to CO2
#' 
#' @param surface Leaf surface (lower or upper)
#' 
#' @rdname CO2_conductance

.get_guc <- function(pars, surface, unitless) {
  
  surface %<>% match.arg(c("lower", "upper"))
  
  if (unitless) {
    g_uc <- switch(
      surface,
      lower = pars$g_uc * (1 / (1 + pars$k_uc)),
      upper = pars$g_uc * (pars$k_uc / (1 + pars$k_uc))
    )
  } else {
    g_uc <- switch(
      surface,
      lower = pars$g_uc * (set_units(1) / (set_units(1) + pars$k_uc)),
      upper = pars$g_uc * (pars$k_uc / (set_units(1) + pars$k_uc))
    )
  }
  
  g_uc
  
}

#'  - g_bc: boundary layer conductance to CO2
#' 
#' @inheritParams .get_guc
#' 
#' @rdname CO2_conductance

.get_gbc <- function(pars, surface, unitless) {
  
  surface %<>% match.arg(c("lower", "upper"))
  
  ret <- tealeaves:::.get_gbw(pars$T_leaf, surface, pars, unitless) %>%
    set_units(m/s) %>%
    gunit::convert_conductance(
      P = set_units(pars$P, kPa),
      R = set_units(pars$R, J/K/mol),
      Temp = set_units((pars$T_air + pars$T_leaf) / 2, K)
    ) %>%
    dplyr::pull(.data$`umol/m^2/s/Pa`) %>%
    gw2gc(D_c = pars$D_c0, D_w = pars$D_w0, unitless = unitless)
  
  ret
  
}

#'  - g_mc: mesophyll conductance to CO2
#' 
#' @inheritParams .get_guc
#' 
#' @rdname CO2_conductance

.get_gmc <- function(pars, surface, unitless) {
  
  surface %<>% match.arg(c("lower", "upper"))
  
  if (unitless) {
    g_mc <- switch(
      surface,
      lower = pars$g_mc * (1 / (1 + pars$k_mc)),
      upper = pars$g_mc * (pars$k_mc / (1 + pars$k_mc))
    )
  } else {
    g_mc <- switch(
      surface,
      lower = pars$g_mc * (set_units(1) / (set_units(1) + pars$k_mc)),
      upper = pars$g_mc * (pars$k_mc / (set_units(1) + pars$k_mc))
    )
  }
  
  g_mc
  
}

#'  - g_sc: stomatal conductance to CO2
#' 
#' @inheritParams .get_guc
#' 
#' @rdname CO2_conductance

.get_gsc <- function(pars, surface, unitless) {
  
  surface %<>% match.arg(c("lower", "upper"))
  if (unitless) {
    g_sc <- switch(
      surface,
      lower = pars$g_sc * (1 / (1 + pars$k_sc)),
      upper = pars$g_sc * (pars$k_sc / (1 + pars$k_sc))
    )
  } else {
    g_sc <- switch(
      surface,
      lower = pars$g_sc * (set_units(1) / (set_units(1) + pars$k_sc)),
      upper = pars$g_sc * (pars$k_sc / (set_units(1) + pars$k_sc))
    )
  }
  
  g_sc
  
}