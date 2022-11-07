#' Conductance to CO2 (mol / m^2 / s)
#'
#' @inheritParams A_supply
#'
#' @name CO2_conductance
#'
#' @details
#' 
#' \bold{Default conductance model}
#' 
#' The conductance model described in this section is used by default unless
#' additional anatomical parameters described in the next section are provided.
#' 
#' Total conductance to CO2 is the sum of parallel conductances on the lower
#' (\eqn{g_\mathrm{c,lower}}{gc_lower}) and upper
#' (\eqn{g_\mathrm{c,upper}}{gc_upper}) leaf portions:
#'
#'  \deqn{g_\mathrm{c,total} = g_\mathrm{c,lower} + g_\mathrm{c,upper}}{gc_total = gc_lower + gc_upper}
#'
#' Each partial conductance consists of two parallel conductances, the
#' cuticular conductance (\eqn{g_\mathrm{u,c}}{g_uc}) and the in-series
#' conductances through mesophyll (\eqn{g_\mathrm{m,c}}{g_mc}), stomata (\eqn{g_\mathrm{s,c}}{g_sc}), and boundary layer (\eqn{g_\mathrm{b,c}}{g_bc}). To simplify the formula, I use substitute resistance where \eqn{r_x = 1 / g_x}. For surface \eqn{i}:
#'
#' \deqn{g_{\mathrm{c},i} = g_{\mathrm{u},i} + (1 / (r_{\mathrm{m},i} + r_{\mathrm{s},i} + r_{\mathrm{b},i}))}{g_ci = g_ui + (1 / (r_mi + r_si + r_bi))}
#'
#' The cuticular, stomatal, and mesophyll conductances can be the same or
#' different for upper and lower. The partitioning factors (\eqn{k_x}) divide the conductance between surfaces while keeping the total conductance constant:
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
#' The boundary layer conductances for each are calculated on the basis of mass 
#' and heat transfer (see \code{\link{.get_gbc}}).
#'
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\mathrm{mc}}{g_mc} \tab \code{g_mc} \tab mesophyll conductance to CO2 (T_leaf) \tab mol / m\eqn{^2} / s \tab \link[=bake]{calculated} \cr
#' \eqn{g_\mathrm{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab mol / m\eqn{^2} / s \tab `r dplyr::pull(photo_parameters[photo_parameters$R == "g_sc","default"])` \cr
#' \eqn{g_\mathrm{uc}}{g_uc} \tab \code{g_uc} \tab cuticular conductance to CO2 \tab mol / m\eqn{^2} / s \tab `r dplyr::pull(photo_parameters[photo_parameters$R == "g_uc","default"])` \cr
#' \eqn{k_\mathrm{mc}}{k_mc} \tab \code{k_mc} \tab partition of \eqn{g_\mathrm{mc}}{g_mc} to lower mesophyll \tab none \tab `r dplyr::pull(photo_parameters[photo_parameters$R == "k_mc","default"])` \cr
#' \eqn{k_\mathrm{sc}}{k_sc} \tab \code{k_sc} \tab partition of \eqn{g_\mathrm{sc}}{g_sc} to lower surface \tab none \tab `r dplyr::pull(photo_parameters[photo_parameters$R == "k_sc","default"])` \cr
#' \eqn{k_\mathrm{uc}}{k_uc} \tab \code{k_uc} \tab partition of \eqn{g_\mathrm{uc}}{g_uc} to lower surface \tab none \tab `r dplyr::pull(photo_parameters[photo_parameters$R == "k_uc","default"])` \cr
#' }
#'
#' \bold{New conductance model}
#' 
#' The conductance model described in this section is implemented in 
#' \bold{photosynthesis} (>= 2.1.0) if parameters to calculate the internal 
#' airspace and liquid-phase conductances (\code{A_mes_A}, \code{g_liqc}) are 
#' provided. These parameters are 1) the effective path lengths through the 
#' lower and upper leaf internal airspaces (\code{delta_ias_lower}, 
#' \code{delta_ias_upper}) and 2) the mesophyll area per leaf area 
#' (\code{A_mes_A}) and liquid-phase conductance per mesophyll cell area 
#' (\code{g_liqc}).
#' 
#' Two parallel diffusion pathways, one from each leaf surface, converge to a 
#' single CO2 concentration at the mesophyll cell boundary. We use a single 
#' liquid-phase resistance to represent the combined cell wall, plasmalemma, and 
#' chloroplast resistances. The gas-phase resistance through boundary layer, 
#' cuticle/stomata, and internal airspace is \eqn{r_\mathrm{gas,c}}; the 
#' liquid-phase intracellular resistance is \eqn{r_\mathrm{i,c}}.
#' 
#'  \deqn{r_\mathrm{total,c} = r_\mathrm{gas,c} + r_\mathrm{i,c}}{r_total,c = r_gas,c + r_i,c}
#'  
#' The gas-phase resistance occurs through two parallel pathways, which we refer 
#' to as the 'lower' and 'upper' pathways because horizontally oriented leaves 
#' often have different anatomical properties on each surface. The gas-phase 
#' resistance through pathway \eqn{i \in \{\textrm{lower,upper\}}} is:
#' 
#' \deqn{r_{\mathrm{gas,c},i} = r_{\mathrm{b,c},i} + r_{\mathrm{u+s,c},i} + r_{\mathrm{ias,c},i}}{r_gas,c,i = r_b,c,i + r_u+s,c,i + r_ias,c,i}
#' 
#' The subscripts \eqn{_\mathrm{b}}, \eqn{_\mathrm{u+s}}, and \eqn{_\mathrm{ias}} 
#' denote boundary layer, cuticular + stomatal, and internal airspace, 
#' respectively. The subscript \eqn{_\mathrm{c}} indicates we are considering 
#' the conductance to CO2 rather than another molecular species.
#' 
#' Cuticular and stomatal conductances (1 / resistance) are parallel, so:
#' 
#' \deqn{1 / r_{\mathrm{u+s,c},i} = g_{\mathrm{u+s,c},i} = g_{\mathrm{u,c},i} + g_{\mathrm{s,c},i}}{1 / r_u+s,c,i = g_u+s,c,i = g_u,c,i + g_s,c,i}
#' 
#' Substituting the above expression into the equation for \eqn{r_{\mathrm{gas,c},i}}{r_gas,c,i}:
#' 
#' \deqn{r_{\mathrm{gas,c},i} = r_{\mathrm{b,c},i} + 1 / (g_{\mathrm{u,c},i} = g_{\mathrm{s,c},i}) + r_{\mathrm{ias,c},i}}{r_gas,c,i = r_b,c,i + 1 / (g_u,c,i + g_s,c,i) + r_ias,c,i}
#' 
#' The total gas-phase resistance is the inverse of the sum of the parallel 
#' lower and upper conductances:
#' 
#' \deqn{1 / r_{\mathrm{gas,c}} = g_\mathrm{gas,c,lower} + g_\mathrm{gas,c,upper}}{1 / r_gas,c = g_gas,c = g_gas,c,lower + g_gas,c,upper}
#' 
#' The cuticular, stomatal, and mesophyll conductances can be the same or
#' different for upper and lower. The partitioning factors \eqn{k_u} and \eqn{k_s} 
#' divide the total cuticular and stomatal conductances, respectively, between 
#' surfaces while keeping the total conductance constant:
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
#' The internal airspace conductance is the diffusivity of CO2 at a given
#' temperature and pressure divided by the effective path length:
#' 
#' \deqn{g_\mathrm{ias,c,lower} = D_\mathrm{c} / \delta_\mathrm{ias,lower}}{g_iasc_lower = D_c / delta_ias_lower}
#' \deqn{g_\mathrm{ias,c,upper} = D_\mathrm{c} / \delta_\mathrm{ias,upper}}{g_iasc_ipper = D_c / delta_ias_upper}
#' 
#' \code{g_iasc_lower} and \code{g_iasc_upper} are calculated in the \link{bake} 
#' function. See \code{\link[tealeaves]{.get_Dx}} for calculating \code{D_c}.
#' 
#' The liquid-phase intracellular resistance is given by:
#' 
#' \deqn{1 / r_\mathrm{i,c} = g_\mathrm{i,c} = g_\mathrm{liq,c} A_\mathrm{mes} / A}{1 / r_i,c = g_i,c = g_liq,c A_mes / A}
#' 
#' \eqn{g_\mathrm{liq,c}}{g_liq,c} is temperature sensitive. See \code{\link{bake}}.
#' 
#' The boundary layer conductances for each are calculated on the basis of mass
#' and heat transfer (see \code{\link{.get_gbc}}).
#' 
#' @encoding UTF-8
#' @md

NULL

#'  - g_tc: total conductance to CO2
#'
#' @rdname CO2_conductance
.get_gtc = function(pars, unitless, use_legacy_version) {

  if (check_new_conductance(pars, baked = TRUE)) {
    
    gbc_lower = .get_gbc(pars, "lower", unitless, use_legacy_version)
    gsc_lower = .get_gsc(pars, "lower", unitless)
    guc_lower = .get_guc(pars, "lower", unitless)
    
    gbc_upper = .get_gbc(pars, "upper", unitless, use_legacy_version)
    gsc_upper = .get_gsc(pars, "upper", unitless)
    guc_upper = .get_guc(pars, "upper", unitless)
    
    g_usc_lower = guc_lower + gsc_lower
    g_usc_upper = guc_upper + gsc_upper
    g_gasc_lower = 1 / (1 / gbc_lower + 1 / g_usc_lower + 1 / pars$g_iasc_lower)
    g_gasc_upper = 1 / (1 / gbc_upper + 1 / g_usc_upper + 1 / pars$g_iasc_upper)
    g_gasc = g_gasc_lower + g_gasc_upper
    g_ic = pars$g_liqc * pars$A_mes_A
    g_tc = 1 / (1 / g_gasc + 1 / g_ic)
    
    if (!unitless) g_tc %<>% set_units(mol / m^2 / s)
    
    return(g_tc)
    
  } else {
    
    gbc_lower = .get_gbc(pars, "lower", unitless, use_legacy_version)
    gmc_lower = .get_gmc(pars, "lower", unitless)
    gsc_lower = .get_gsc(pars, "lower", unitless)
    guc_lower = .get_guc(pars, "lower", unitless)
    
    gbc_upper = .get_gbc(pars, "upper", unitless, use_legacy_version)
    gmc_upper = .get_gmc(pars, "upper", unitless)
    gsc_upper = .get_gsc(pars, "upper", unitless)
    guc_upper = .get_guc(pars, "upper", unitless)
    
    rc_lower = 1 / gmc_lower + 1 / gsc_lower + 1 / gbc_lower
    gc_lower = 1 / rc_lower
    gc_lower %<>% magrittr::add(guc_lower)
    rc_upper = 1 / gmc_upper + 1 / gsc_upper + 1 / gbc_upper
    gc_upper = 1 / rc_upper
    gc_upper %<>% magrittr::add(guc_upper)
    
    g_tc = gc_lower + gc_upper
    
    if (!unitless) g_tc %<>% set_units(mol / m^2 / s)
    
    return(g_tc)
    
  }
  
}

#'  - g_uc: cuticular conductance to CO2
#'
#' @param surface Leaf surface (lower or upper)
#'
#' @rdname CO2_conductance
.get_guc = function(pars, surface, unitless) {
  surface %<>% match.arg(c("lower", "upper"))

  if (unitless) {
    g_uc = switch(
      surface,
      lower = pars$g_uc * (1 / (1 + pars$k_uc)),
      upper = pars$g_uc * (pars$k_uc / (1 + pars$k_uc))
    )
  } else {
    g_uc = switch(
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
.get_gbc = function(pars, surface, unitless, use_legacy_version) {
  surface %<>% match.arg(c("lower", "upper"))

  # Hack because f_sh = sh_constant, f_sh = sh_constant in tealeaves
  # Should update tealeaves to harmonize variable and function names
  pars$sh_constant = pars$f_sh
  pars$nu_constant = pars$f_nu
  ret = tealeaves:::.get_gbw(pars$T_leaf, surface, pars, unitless) |>
      set_units(m / s) |>
      gunit::convert_conductance(
        P = set_units(pars$P, kPa),
        R = set_units(pars$R, J / K / mol),
        Temp = set_units((pars$T_air + pars$T_leaf) / 2, K)
      ) |>
      dplyr::pull(.data$`umol/m^2/s/Pa`) |>
      gunit::gw2gc(D_c = pars$D_c0, D_w = pars$D_w0, unitless = unitless,
            a = ifelse(use_legacy_version, 1, 2/3)) |>
    # Convert to mol / m^2 / s
    magrittr::multiply_by(pars$P) %>%
    purrr::when(
      # Divide 1e3 because conversion is from umol / kPa -> mol
      # umol / m^2 / s / Pa * 1e3 Pa / kPa * mol / 1e6 umol
      unitless ~ . / 1e3,
      !unitless ~ set_units(., mol/m^2/s)
    )
  ret
  
}
#'  - g_mc: mesophyll conductance to CO2
#'
#' @inheritParams .get_guc
#'
#' @rdname CO2_conductance
.get_gmc = function(pars, surface, unitless) {
  
  surface %<>% match.arg(c("lower", "upper"))

    if (unitless) {
      g_mc = switch(
        surface,
        lower = pars$g_mc * (1 / (1 + pars$k_mc)),
        upper = pars$g_mc * (pars$k_mc / (1 + pars$k_mc))
      )
    } else {
      g_mc = switch(
        surface,
        lower = pars$g_mc * (1 / (set_units(1) + pars$k_mc)),
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
.get_gsc = function(pars, surface, unitless) {
  surface %<>% match.arg(c("lower", "upper"))
  if (unitless) {
    g_sc = switch(
      surface,
      lower = pars$g_sc * (1 / (1 + pars$k_sc)),
      upper = pars$g_sc * (pars$k_sc / (1 + pars$k_sc))
    )
  } else {
    g_sc = switch(
      surface,
      lower = pars$g_sc * (set_units(1) / (set_units(1) + pars$k_sc)),
      upper = pars$g_sc * (pars$k_sc / (set_units(1) + pars$k_sc))
    )
  }

  g_sc
}
