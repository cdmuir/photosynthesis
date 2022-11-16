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
#' @param check Logical. Should arguments checks be done? Default is TRUE.
#' 
#' @param parallel Logical. Should parallel processing be used via \code{\link[furrr]{future_map}}?
#' 
#' @param use_legacy_version Logical. Should legacy model (<2.1.0) be used? See \href{https://github.com/cdmuir/photosynthesis/blob/master/NEWS.md}{NEWS} for further information. Default is FALSE. 
#'
#' @return
#' A data.frame with the following \code{units} columns \cr
#' 
#' \bold{Inputs:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(!temperature_response, !tealeaves)
#' ```
#' \bold{Baked Inputs:} 
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(temperature_response, !tealeaves)
#' ```
#'
#' \tabular{ll}{
#'
#' \bold{Output:} \tab \cr
#' \cr
#' \code{A} \tab photosynthetic rate at \code{C_chl} (\eqn{\mu}mol CO2 / m\eqn{^2} / s) \cr
#' \code{C_chl} \tab chloroplastic CO2 concentration where \code{A_supply} intersects \code{A_demand} (\eqn{mu}mol / mol) \cr
#' \code{C_i} \tab intercellular CO2 concentration where \code{A_supply} intersects \code{A_demand} (\eqn{mu}mol / mol) \cr
#' \code{g_tc} \tab total conductance to CO2 at \code{T_leaf} (mol / m\eqn{^2} / s)) \cr
#' \code{value} \tab \code{A_supply} - \code{A_demand} (\eqn{\mu}mol / (m\eqn{^2} s)) at \code{C_chl} \cr
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
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(use_tealeaves = FALSE)
#' photo(leaf_par, enviro_par, bake_par, constants,
#'   use_tealeaves = FALSE
#' )
#'
#' # Multiple parameter sets with 'photosynthesis'
#'
#' leaf_par = make_leafpar(
#'   replace = list(
#'     T_leaf = set_units(c(293.14, 298.15), "K")
#'   ), use_tealeaves = FALSE
#' )
#' photosynthesis(leaf_par, enviro_par, bake_par, constants,
#'   use_tealeaves = FALSE
#' )
#' @encoding UTF-8
#'
#' @export
#' @md

photosynthesis = function(
    leaf_par, 
    enviro_par, 
    bake_par, 
    constants,
    use_tealeaves, 
    progress = TRUE, 
    quiet = FALSE,
    assert_units = TRUE, 
    check = TRUE,
    parallel = FALSE,
    use_legacy_version = FALSE
) {
  
  # Check arguments ----
  checkmate::assert_flag(check)
  
  if (check) {
    checkmate::assert_class(bake_par, "bake_par")
    checkmate::assert_class(constants, "constants")
    checkmate::assert_class(enviro_par, "enviro_par")
    checkmate::assert_class(leaf_par, "leaf_par")
    checkmate::assert_flag(use_tealeaves)
    checkmate::assert_flag(quiet)
    checkmate::assert_flag(assert_units)
    checkmate::assert_flag(parallel)
    checkmate::assert_flag(use_legacy_version)
  }
  
  # Message about legacy version ----
  notify_users(quiet = quiet, leaf_par = leaf_par)
  
  T_air = NULL
  if (!use_tealeaves && !is.null(enviro_par$T_air)) {
    if (!quiet) {
      message(glue::glue("Both air and leaf temperature are provided and fixed: T_air = {T_air}; T_leaf = {T_leaf}",
                         T_air = enviro_par$T_air,
                         T_leaf = leaf_par$T_leaf
      ))
    }
    T_air = enviro_par$T_air
  }
  
  # Assert units ----
  if (assert_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% photosynthesis::constants(use_tealeaves)
    enviro_par %<>% photosynthesis::enviro_par(use_tealeaves)
    leaf_par %<>% photosynthesis::leaf_par(use_tealeaves)
    if (!is.null(T_air)) enviro_par$T_air = set_units(T_air, K)
  }
  
  # Make parameter sets ----
  pars = make_parameter_sets(leaf_par, enviro_par, bake_par, constants)
  
  # Solve ----
  soln = solve_for_photosynthesis(
    pars,
    bake_par, 
    constants, 
    use_tealeaves,
    progress,
    quiet,
    parallel,
    use_legacy_version
  )
  
  # Return ----
  soln
  
}

#' Make parameter sets for \code{\link{photosynthesis}}
#' @inheritParams photosynthesis
#' @noRd
make_parameter_sets = function(
    leaf_par, 
    enviro_par, 
    bake_par, 
    constants
) {
  
  ## cross_df() removes units
  pars = c(
    purrr::keep(leaf_par, ~ length(.x) > 0), 
    purrr::keep(enviro_par, ~ length(.x) > 0)
  ) |>
    purrr::map(~ {if (is.function(.x)) {list(.x)} else {.x}}) |>
    purrr::cross_df()
  
  ## Add units back
  function_pars = apply(pars, 2, function(.x) any(sapply(.x, is.function)))
  function_par_cols = pars[, function_pars]
  pars = pars %>% 
    set_parameter_units(.data$R %in% colnames(.)[!function_pars]) |>
    tibble::as_tibble() |>
    dplyr::bind_cols(function_par_cols)
  
  pars
  
}

#' Solve for C_chl and A for each parameter set within \code{\link{photosynthesis}}
#' @inheritParams photosynthesis
#' @noRd
solve_for_photosynthesis = function(
    pars,
    bake_par, 
    constants, 
    use_tealeaves,
    progress,
    quiet,
    parallel,
    use_legacy_version
) {
  
  if (!quiet) {
    glue::glue("\nSolving for photosynthetic rate from {n} parameter set{s} ...",
               n = nrow(pars), s = dplyr::if_else(length(pars) > 1, "s", "")
    ) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (progress && !parallel) pb = dplyr::progress_estimated(nrow(pars))
  
  soln = if (parallel) {
    pars %>%
      split(~ seq_len(nrow(.))) |>
      furrr::future_map_dfr(
        solve_for_photosynthesis_set, 
        bake_par = bake_par,
        constants = constants,
        use_tealeaves = use_tealeaves,
        use_legacy_version = use_legacy_version,
        .progress = progress
      )
  } else {
    pars %>%
      split(~ seq_len(nrow(.))) |>
      purrr::map_dfr(~ {
        ret = solve_for_photosynthesis_set(
          pars = .x,
          bake_par = bake_par,
          constants = constants,
          use_tealeaves = use_tealeaves,
          use_legacy_version = use_legacy_version
        )
        if (progress) pb$tick()$print()
        ret
      })
    }
  
  soln
  
}

#' Solve for C_chl and A for a single parameter set within \code{\link{photosynthesis}}
#' @inheritParams photosynthesis
#' @noRd
solve_for_photosynthesis_set = function(
    pars,
    bake_par, 
    constants, 
    use_tealeaves,
    use_legacy_version
) {
  
  lx = intersect(
    colnames(pars), 
    parameter_names("leaf", use_tealeaves = use_tealeaves)
  )
  
  ex = intersect(
    colnames(pars),
    parameter_names("enviro", use_tealeaves = use_tealeaves)
  )
  
  # This would cause an error is element was list with multiple elements,
  # but this structure shouldn't occur by this point
  lp = as.list(pars)[lx] |>
    lapply(function(.x) if (is.list(.x)) {.x[[1]]} else .x)
  
  ep = as.list(pars)[ex] |>
    lapply(function(.x) if (is.list(.x)) {.x[[1]]} else .x)

  photo(lp, ep, bake_par, constants, use_tealeaves, quiet = TRUE,
        assert_units = FALSE, check = FALSE,
        use_legacy_version = use_legacy_version)
  
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

photo = function(
    leaf_par, 
    enviro_par, 
    bake_par, 
    constants,
    use_tealeaves, 
    quiet = FALSE, 
    assert_units = TRUE,
    check = TRUE, 
    prepare_for_tleaf = use_tealeaves,
    use_legacy_version = FALSE
  ) {
  
  # Check arguments ----
  checkmate::assert_flag(check)

  if (check) {
    checkmate::assert_class(bake_par, "bake_par")
    checkmate::assert_class(constants, "constants")
    checkmate::assert_class(enviro_par, "enviro_par")
    checkmate::assert_class(leaf_par, "leaf_par")
    checkmate::assert_flag(use_tealeaves)
    checkmate::assert_flag(quiet)
    checkmate::assert_flag(assert_units)
    checkmate::assert_flag(prepare_for_tleaf)
    checkmate::assert_flag(use_legacy_version)
  }

  # Message about legacy version ----
  notify_users(quiet = quiet, leaf_par = leaf_par)
  
  T_air = NULL
  if (!use_tealeaves && !is.null(enviro_par$T_air)) {
    if (!quiet) {
      message(glue::glue("Both air and leaf temperature are provided and fixed: T_air = {T_air}; T_leaf = {T_leaf}",
        T_air = enviro_par$T_air,
        T_leaf = leaf_par$T_leaf
      ))
    }
    T_air = enviro_par$T_air
  }

  # Set units and bake ----
  if (assert_units) {
    bake_par %<>% photosynthesis::bake_par()
    constants %<>% photosynthesis::constants(use_tealeaves)
    enviro_par %<>% photosynthesis::enviro_par(use_tealeaves)
    leaf_par %<>% photosynthesis::leaf_par(use_tealeaves)
    if (!is.null(T_air)) enviro_par$T_air = set_units(T_air, K)
  }

  # Calculate T_leaf using energy balance ----
  if (use_tealeaves) {
    leaf_par %<>% add_Tleaf_photo(enviro_par, constants, prepare_for_tleaf)
  }
  
  leaf_par %<>% bake(enviro_par, bake_par, constants, assert_units = FALSE)

  pars = c(leaf_par, enviro_par, constants) %>%
    purrr::map_if(~ inherits(.x, "units"), drop_units)
  if (!use_tealeaves && is.null(pars$T_air)) pars$T_air = pars$T_leaf

  # Find intersection between photosynthetic supply and demand curves -----
  soln = find_A(pars, quiet, use_legacy_version)

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
  shared_pars = intersect(names(leaf_par), names(enviro_par))
  checkmate::assert_true(all(unlist(leaf_par[shared_pars]) ==
    unlist(enviro_par[shared_pars])))
  leaf_par[shared_pars] = NULL

  soln = c(
    soln,
    purrr::keep(leaf_par, ~ length(.x) > 0 & !is.function(.x)),
    purrr::keep(enviro_par, ~ length(.x) > 0 & !is.function(.x)),
    purrr::keep(bake_par, ~ length(.x) > 0 & !is.function(.x)),
    purrr::keep(constants, ~ length(.x) > 0 & !is.function(.x))
  ) |>
    as.data.frame()
  
  soln$C_chl %<>% set_units(umol / mol)
  soln$g_tc %<>% set_units(mol / m^2 / s)
  soln$A %<>% set_units(umol / m^2 / s)
  soln$C_i = set_units(soln$C_air - soln$A / soln$g_sc, umol/mol)
  
  soln
  
}

#' Calculate leaf temperature using \code{\link[tealeaves]{tleaf}}
#' @inheritParams photo
#' @noRd
add_Tleaf_photo = function(leaf_par, enviro_par, constants, prepare_for_tleaf) {
  
  leaf_par1 = leaf_par
  constants1 = constants
  
  if (prepare_for_tleaf) {
    enviro_par$S_sw = set_units(enviro_par$E_q * enviro_par$PPFD /
                                  enviro_par$f_par, W / m^2)
    leaf_par$g_sw = set_units(
      constants$D_w0 / constants$D_c0 * leaf_par$g_sc,
      mol / m^2 / s
    )
    leaf_par$g_uw = set_units(
      constants$D_w0 / constants$D_c0 * leaf_par$g_uc,
      mol / m^2 / s
    )
    
    leaf_par$logit_sr = if (is(leaf_par$k_sc, "units")) {
      stats::qlogis(leaf_par$k_sc / (set_units(1) + leaf_par$k_sc))
    } else {
      stats::qlogis(leaf_par$k_sc / (1 + leaf_par$k_sc))
    }
    
    # Need this until tealeaves changes these parameters for consistency
    leaf_par1$g_sw = if (is(leaf_par$g_sw, "units")) {
      leaf_par$g_sw |>
        gunit::convert_conductance(P = enviro_par$P, R = constants$R) |>
        magrittr::extract2("umol/m^2/s/Pa")
    } else {
      leaf_par$g_sw / enviro_par$P * 1000
    }
    
    leaf_par1$g_uw = if (is(leaf_par$g_uw, "units")) {
      leaf_par$g_uw |>
        gunit::convert_conductance(P = enviro_par$P, R = constants$R) |>
        magrittr::extract2("umol/m^2/s/Pa")
    } else {
      leaf_par$g_uw / enviro_par$P * 1000
    }
    
    
    constants1$nu_constant = constants$f_nu
    constants1$sh_constant = constants$f_sh
    constants1$f_nu = constants1$f_sh = NULL
    constants1$s = constants1$sigma
    constants1$sigma = NULL
  }
  
  tl = tealeaves::tleaf(
    leaf_par = leaf_par1, 
    enviro_par = enviro_par,
    constants = constants1, 
    quiet = TRUE,
    set_units = TRUE
  ) %>% 
    dplyr::rename(
      tealeaves_convergence = "convergence",
      tealeaves_value = "value"
    )
  leaf_par$T_leaf = tl$T_leaf

  leaf_par
  
}

supply_minus_demand = function(C_chl, unitless_pars, use_legacy_version) {
  supply = A_supply(C_chl, unitless_pars, unitless = TRUE, use_legacy_version)
  demand = A_demand(C_chl, unitless_pars, unitless = TRUE)
  supply - demand
}

find_A = function(unitless_pars, quiet, use_legacy_version) {

    if (!quiet) {
    "\nSolving for C_chl ..." %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }

  fit = tryCatch(
    {
      Cchl_upper = max(c(unitless_pars$gamma_star, unitless_pars$C_air))
      while (supply_minus_demand(Cchl_upper, unitless_pars, use_legacy_version) > 0) {
        Cchl_upper = 2 * Cchl_upper
      }
      stats::uniroot(supply_minus_demand,
        unitless_pars = unitless_pars, lower = 0.1,
        upper = Cchl_upper, 
        check.conv = TRUE, use_legacy_version = use_legacy_version
      )
    },
    finally = {
      fit = list(root = NA, f.root = NA, convergence = 1)
    }
  )

  soln = data.frame(
    C_chl = fit$root, value = fit$f.root,
    convergence = dplyr::if_else(is.null(fit$convergence), 0, 1)
  )

  if (!quiet) {
    " done" %>%
      crayon::green() %>%
      message()
  }

  soln$g_tc = .get_gtc(unitless_pars, unitless = TRUE, use_legacy_version)
  soln$A = A_supply(soln$C_chl, unitless_pars, unitless = TRUE,
                    use_legacy_version)

  soln
}

#' CO2 supply and demand function (mol / m^2 s)
#'
#' This function is not intended to be called by users directly.
#' 
#' @inheritParams photosynthesis
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
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(use_tealeaves = FALSE)
#' leaf_par = bake(leaf_par, enviro_par, bake_par, constants)
#' # Or bake with piping (need library(magrittr))
#' # leaf_par %<>% bake(enviro_par, bake_par, constants)
#' enviro_par$T_air = leaf_par$T_leaf
#'
#' pars = c(leaf_par, enviro_par, constants)
#' C_chl = set_units(350, umol/mol)
#'
#' A_supply(C_chl, pars)
#'
#' A_demand(C_chl, pars)
#' @export

A_supply = function(C_chl, pars, unitless = FALSE, use_legacy_version = FALSE) {
  g_tc = .get_gtc(pars, unitless, use_legacy_version)

  if (unitless) {
    As = g_tc * (pars$C_air - C_chl)
  } else {
    As = set_units(g_tc * (pars$C_air - C_chl), umol / m^2 / s)
  }
  As
}

#' A_demand
#' @rdname A_supply
#' @export

A_demand = function(C_chl, pars, unitless = FALSE) {
  if (unitless) {
    Ad = (1 - pars$gamma_star / C_chl) * FvCB(C_chl, pars, unitless)$A - pars$R_d
  } else {
    Ad = set_units((set_units(1) - pars$gamma_star / C_chl) *
      FvCB(C_chl, pars, unitless)$A - pars$R_d, umol / m^2 / s)
  }

  Ad
}

#' Check whether users supplied parameters to calculate g_ias and g_liq
#' @noRd
check_new_conductance = function(pars, baked) {
  checkmate::assert_flag(baked)
  if (baked) {
    c("g_iasc_lower", "g_iasc_upper", "A_mes_A", "g_liqc") |>
      purrr::map_lgl(function(.x, pars) {
        length(pars[[.x]]) > 0 & all(!is.na(pars[[.x]]))
      }, pars = pars) |>
      all()
  } else {
    c("delta_ias_lower", "delta_ias_upper", "A_mes_A", "g_liqc25") |>
      purrr::map_lgl(function(.x, pars) {
        length(pars[[.x]]) > 0 & all(!is.na(pars[[.x]]))
      }, pars = pars) |>
      all()
  }
  
}

#' Notify users about important changes in \link[photosyntesis]
#' @noRd
notify_users = function(quiet, leaf_par) {
  
  if (!quiet) {
    message("
            
  As of version 2.1.0, the CO2 conductance model changed slightly. 
  To implement legacy version, use:
  
  `> photosynthesis(..., use_legacy_version = TRUE)`.")
    
    if (check_new_conductance(leaf_par, baked = FALSE)) {
      message("
  It looks like you provided parameters to calculate g_ias and g_liq.
  The parameters g_mc and k_mc will be ignored and calculated from g_ias 
  and g_liq. This is a new feature in version 2.1.0 and may change in the
  near future. Inspect results carefully.
  ")
    }
    
  }
  
}
