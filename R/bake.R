#' S3 class baked
#' @name baked-class
#' @description See [bake()]

NULL

#' Leaf parameter temperature responses
#'
#' @description 'bake' leaf parameters using temperature response functions
#'
#' @name bake
#'
#' @inheritParams photosynthesis
#' @inheritParams A_supply
#' 
#' @returns 
#'
#' Constructor function for `baked` class. This will also inherit class
#' [leaf_par()] and [list()]. This function ensures that 
#' temperature is "baked in" to leaf parameter calculations `T_leaf` using 
#' temperature response functions detailed below.
#'
#' @details
#'
#' Several leaf parameters ([leaf_par()]) are temperature sensitive.
#' Temperature-sensitive parameters are input at a reference temperature of 
#' 25 °C. These parameters are provided as `par_name25` and then "baked"
#' using the appropriate temperature response function and parameters in
#' [bake_par()]. The "baked" parameter will have the name without "25"
#' appended (`par_name`). E.g. `V_cmax25` becomes `V_cmax`. \cr
#' \cr
#' Temperature response functions following Buckley and  Diaz-Espejo (2015) \cr
#' \cr
#' Temperature response function 1 (`temp_response1`): \cr
#'
#' \deqn{\mathrm{par}(T_\mathrm{leaf}) = \mathrm{par25}~\mathrm{exp}(E_\mathrm{a} / (R T_\mathrm{ref}) (T_\mathrm{leaf} - 25) / (T_\mathrm{leaf} + 273.15))}{par(T_leaf) = par25 exp(E_a / (R T_ref) (T_leaf - 25) / (T_leaf + 273.15))}
#'
#' \eqn{T_\mathrm{ref}}{T_ref} is the reference temperature in K \cr
#' \eqn{T_\mathrm{leaf}}{T_leaf} is the leaf temperature in °C \cr
#' \cr
#' Temperature response function 2 (`temp_response2`) is the above equation multiplied by: \cr
#'
#' \deqn{(1 + \mathrm{exp}((D_\mathrm{s} / R - E_\mathrm{d} / (R T_\mathrm{ref})))) / (1 + \mathrm{exp}((D_\mathrm{s} / R) - (E_\mathrm{d} / (R (T_\mathrm{leaf} + 273.15)))))}{(1 + exp((D_s / R - E_d / (R T_ref)))) / (1 + exp((D_s / R) - (E_d / (R (T_leaf + 273.15)))))}
#'
#' Function 1 increases exponentially with temperature; Function 2 peaks a particular temperature.
#'
#' @encoding UTF-8
#'
#' @references
#'
#' Buckley TN, Diaz-Espejo A. 2015. Partitioning changes in photosynthetic rate 
#' into contributions from different variables. Plant, Cell and Environment 38:
#' 1200-1211.
#'
#' @examples
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(
#'   replace = list(T_leaf = set_units(293.15, K)),
#'   use_tealeaves = FALSE
#' )
#' baked_leafpar = bake(leaf_par, enviro_par, bake_par, constants)
#'
#' baked_leafpar$V_cmax25
#' baked_leafpar$V_cmax
#' @encoding UTF-8
#'
#' @export

bake = function(
    leaf_par, 
    enviro_par, 
    bake_par, 
    constants, 
    assert_units = TRUE
  ) {
  
  # Assert units before baking ----
  if (assert_units) {
    leaf_par %<>% leaf_par(use_tealeaves = FALSE)
    enviro_par %<>% enviro_par(use_tealeaves = FALSE)
    bake_par %<>% bake_par()
    constants %<>% constants(use_tealeaves = FALSE)
  }

  # Remove units prior to baking ----
  pars = c(leaf_par, enviro_par, bake_par, constants) |>
    purrr::map_if(~ inherits(.x, "units"), drop_units)
  T_ref = 298.15
  
  # Calculate parameters at T_leaf based on temperature response function ----
  # Assumes that g_liqc has same temperature response function as g_mc
  if (length(pars$g_liqc25) != 0) {
    leaf_par$g_liqc = temp_resp2(
      pars$g_liqc25, pars$Ds_gmc, pars$Ea_gmc, pars$Ed_gmc, pars$R, pars$T_leaf, 
      T_ref, unitless = TRUE
    )
  }
  
  if (length(leaf_par$delta_ias_lower) != 0 & length(leaf_par$delta_ias_upper) != 0) {
    D_c = tealeaves:::.get_Dx(pars$D_c0, pars$T_leaf, pars$eT, pars$P,
                              unitless = TRUE)
    leaf_par$g_iasc_lower = 1e9 * D_c / pars$delta_ias_lower * 
      pars$P / (pars$R * pars$T_leaf)
    leaf_par$g_iasc_upper = 1e9 * D_c / pars$delta_ias_upper * 
      pars$P / (pars$R * pars$T_leaf)
  }
  
  if (length(pars$g_mc25) != 0) {
    leaf_par$g_mc = temp_resp2(
      pars$g_mc25, pars$Ds_gmc, pars$Ea_gmc, pars$Ed_gmc, pars$R, pars$T_leaf, 
      T_ref, unitless = TRUE
    )
  }
  
  leaf_par$gamma_star = temp_resp1(pars$gamma_star25, pars$Ea_gammastar,
    pars$R, pars$T_leaf, T_ref,
    unitless = TRUE
  )
  leaf_par$J_max = temp_resp2(pars$J_max25, pars$Ds_Jmax, pars$Ea_Jmax,
    pars$Ed_Jmax, pars$R, pars$T_leaf, T_ref,
    unitless = TRUE
  )
  leaf_par$K_C = temp_resp1(pars$K_C25, pars$Ea_KC, pars$R, pars$T_leaf,
    T_ref,
    unitless = TRUE
  )
  leaf_par$K_O = temp_resp1(pars$K_O25, pars$Ea_KO, pars$R, pars$T_leaf,
    T_ref,
    unitless = TRUE
  )
  leaf_par$R_d = temp_resp1(pars$R_d25, pars$Ea_Rd, pars$R, pars$T_leaf,
    T_ref,
    unitless = TRUE
  )
  leaf_par$V_cmax = temp_resp1(pars$V_cmax25, pars$Ea_Vcmax, pars$R,
    pars$T_leaf, T_ref,
    unitless = TRUE
  )
  leaf_par$V_tpu = temp_resp1(pars$V_tpu25, pars$Ea_Vtpu, pars$R, pars$T_leaf,
    T_ref,
    unitless = TRUE
  )
  
  # Set units ----
  leaf_par = set_parameter_units(leaf_par, .data$R %in% names(leaf_par))

  # Assert bounds on values ----
  # If !assert_units, no assertion is performed
  if (assert_units) {
    leaf_par |>
      assert_parameter_bounds(
        .data$type == "leaf", 
        .data$temperature_response,
        !.data$tealeaves
      )
  }
  
  leaf_par %<>% structure(class = c("baked", "leaf_par", "list"))

  leaf_par
}

#' Temperature response function 1
#'
#' @rdname bake
#'
#' @param par25 Parameter value at 25 °C of class `units`.
#' @param E_a Empirical temperature response value in J/mol of class
#' `units`.
#' @param R Ideal gas constant in J / (mol K) of class `units`. See
#' [make_constants()].
#' @param T_leaf Leaf temperature in K of class `units`. Will be converted
#' to °C.
#' @param T_ref Reference temperature in K of class `units`.
#'
#' @export

temp_resp1 = function(par25, E_a, R, T_leaf, T_ref, unitless) {
  if (unitless) {
    T_leaf %<>% magrittr::subtract(273.15)
  } else {
    pars_unit = units(par25)
    par25 %<>% drop_units()

    E_a %<>% set_units(J / mol) %>% drop_units()
    R %<>% set_units(J / K / mol) %>% drop_units()
    T_leaf %<>% set_units(degreeC) %>% drop_units()
    T_ref %<>% set_units(K) %>% drop_units()
  }

  a1 = exp(E_a / (R * T_ref) * ((T_leaf - 25) / (T_leaf + 273.15)))

  ret = par25 * a1
  if (!unitless) units(ret) = pars_unit
  ret
}

#' Temperature response function 2
#'
#' @rdname bake
#'
#' @inheritParams temp_resp1
#' @param D_s Empirical temperature response value in J / (mol K) of class
#' `units`.
#' @param E_d Empirical temperature response value in J/mol of class
#' `units`.
#'
#' @export

temp_resp2 = function(par25, D_s, E_a, E_d, R, T_leaf, T_ref, unitless) {
  a1 = temp_resp1(par25, E_a, R, T_leaf, T_ref, unitless)

  if (unitless) {
    T_leaf %<>% magrittr::subtract(273.15)
  } else {
    pars_unit = units(par25)
    par25 %<>% drop_units()
    a1 %<>% drop_units()

    D_s %<>% set_units(J / mol / K) %>% drop_units()
    E_a %<>% set_units(J / mol) %>% drop_units()
    E_d %<>% set_units(J / mol) %>% drop_units()
    R %<>% set_units(J / K / mol) %>% drop_units()
    T_leaf %<>% set_units(degreeC) %>% drop_units()
    T_ref %<>% set_units(K) %>% drop_units()
  }

  a2 = (1 + exp((D_s / R - E_d / (R * T_ref)))) /
    (1 + exp((D_s / R) - (E_d / (R * (T_leaf + 273.15)))))

  ret = a1 * a2
  if (!unitless) units(ret) = pars_unit
  ret
}
