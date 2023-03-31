#' Simulate gas exchange data with measurement error
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' @param ph_out A data frame of output from [photo()] or [photosynthesis()] 
#' with units.
#' @param chamber_pars A data frame with a single row of chamber parameters. 
#' Required parameters are:
#' 
#' * `flow` (\eqn{\mu}mol / s): chamber flow rate
#' * `leaf_area` \[cm ^ 2\]: leaf area in chamber
#' * `sigma_CO2_s` \[\eqn{\mu}mol / mol\]: standard deviation of sample \[CO\eqn{_2}\] measurement error
#' * `sigma_CO2_r` \[\eqn{\mu}mol / mol\]: standard deviation of reference [CO\eqn{_2}\]
#' * `sigma_H2O_s` \[mmol / mol\]: standard deviation of sample \[H\eqn{_2}O\] measurement error
#' * `sigma_H2O_r` \[mmol / mol\]: standard deviation of sample \[H\eqn{_2}O\] measurement error
#' 
#' Units for `flow` and `leaf_area` should be provided; units are implied for
#' sigma's but not necessary to specify because `rnorm()` drop units.
#' 
#' @param n Integer. Number of replicated simulations per row of `ph_out`.
#' @param use_tealeaves Flag. The **tealeaves** package uses a slightly 
#' different equation to calculate the saturating water content of air as a 
#' function temperature and pressure than LI-COR. If FALSE, the function uses 
#' LI-COR's equation in the LI6800 manual. If TRUE, it uses the **tealeaves** 
#' function for internal consistency. The function attempts to guess whether 
#' `ph_out` was run with **tealeaves**, but this can be manually overridden by 
#' providing a value for the argument.
#' 
#' @return A data frame with `n * nrow(ph_out)` rows. It contains all the 
#' original output in `ph_out` as well as a column `.rep` indicating replicate 
#' number from 1 to `n`. Other new columns are assumed or measured chamber 
#' parameters and 'measured' values estimated from synthetic data with 
#' measurement error:
#' 
#' | column name | assumed or derived? | description |
#' |-------------|---------------------|-------------|
#' | `flow`      | assumed             | chamber flow rate |
#' | `leaf_area` | assumed             | leaf area in chamber |
#' | `sigma_CO2_r` | assumed           | standard deviation of measurement error in CO2_r |
#' | `sigma_CO2_s` | assumed           | standard deviation of measurement error in CO2_s |
#' | `sigma_H2O_r` | assumed           | standard deviation of measurement error in H2O_r |
#' | `sigma_H2O_s` | assumed           | standard deviation of measurement error in H2O_s |
#' | `c_0`       | derived             | CO\eqn{_2} concentration before entering chamber \[\eqn{\mu}mol / mol\] |
#' | `w_i`       | derived             | Water vapor concentration within leaf \[mmol / mol\] |
#' | `w_a`       | derived             | Water vapor concentration in chamber \[mmol / mol\] |
#' | `w_0`       | derived             | Water vapor concentration before entering chamber \[mmol / mol\] |
#' | `g_tw`      | derived             | Leaf conductance to water vapor \[mol/m\eqn{^2}/s\] |
#' | `E_area`    | derived             | Evaporation rate per area \[mmol/m\eqn{^2}/s\] |
#' | `E`         | derived             | Total evaporation rate \[mmol/s\] |
#' | `CO2_r`       | derived             | CO\eqn{_2} concentration before entering chamber with measurement error \[\eqn{\mu}mol / mol\] |
#' | `CO2_s`       | derived             | CO\eqn{_2} concentration in chamber with measurement error \[\eqn{\mu}mol / mol\] |
#' | `H2O_s`       | derived           | Water vapor concentration in chamber with measurement error \[mmol / mol\] |
#' | `H2O_r`       | derived           | Water vapor concentration before entering chamber with measurement error \[mmol / mol\] |
#' | `E_meas`        | derived             | Total evaporation rate (measured) \[mmol/s\] |
#' | `E_area_meas`   | derived             | Evaporation rate per area (measured) \[mmol/m\eqn{^2}/s\] |
#' | `g_tw_meas`     | derived             | Leaf conductance to water vapor (measured) \[mol/m\eqn{^2}/s\] |
#' | `g_sc_meas`     | derived             | Stomatal conductance to CO\eqn{_2} (measured) \[mol/m\eqn{^2}/s\] |
#' | `g_tc_meas`     | derived             | Leaf conductance to CO\eqn{_2} (measured) \[mol/m\eqn{^2}/s\] |
#' | `A_meas`        | derived             | Net photosynthetic CO\eqn{_2} assimilation (measured) \[\eqn{\mu}mol/m\eqn{^2}/s\] |
#' | `C_i`           | derived             | Intercellular CO\eqn{_2} concentration (measured) \[\eqn{\mu}mol/mol\] |
#' 
#' @note 
#' To evaluate the accuracy and precision of parameter estimation methods, it 
#' may be useful to simulate data with realistic measurement error. This 
#' function takes output from from [photo()] or [photosynthesis()] models, adds 
#' measurement error in CO\eqn{_2} and H\eqn{_2}O concentrations, and calculates 
#' parameter estimates with synthetic data. Currently, the function assumes a 
#' simplified 1-dimensional CO\eqn{_2} and H\eqn{_2}O conductance model: zero 
#' cuticular conductance, infinite boundary layer conductance, and infinite 
#' airspace conductance. Other assumptions include:
#' 
#' * chamber flow rate, leaf area, leaf temperature, and air pressure are known
#' without error
#' * measurement error is normally distributed mean 0 and standard deviation 
#' specified in `chamber_pars`
#' 
#' This function was designed with the LI-COR LI6800 instrument in mind, but in
#' principle applies to any open path gas exchange system.
#' 
#' @examples 
#' library(photosynthesis)
#' 
#' # Use photosynthesis() to simulate 'real' values
#' # `replace = ...` sets parameters to meet assumptions of `simulate_error()`
#' lp = make_leafpar(replace = list(
#'   g_sc = set_units(0.1, mol/m^2/s),
#'   g_uc = set_units(0, mol/m^2/s),
#'   k_mc = set_units(0, 1),
#'   k_sc = set_units(0, 1),
#'   k_uc = set_units(0, 1)
#'   ),
#'   use_tealeaves = FALSE)
#'    
#'  ep = make_enviropar(replace = list(
#'    wind = set_units(Inf, m/s)
#'  ), use_tealeaves = FALSE) 
#'  bp = make_bakepar()
#'  cs = make_constants(use_tealeaves = FALSE)
#'  
#'  chamber_pars = data.frame(
#'    flow = set_units(600, umol / s),
#'    leaf_area = set_units(6, cm ^ 2),
#'    sigma_CO2_s = 0.1,
#'    sigma_CO2_r = 0.1,
#'    sigma_H2O_s = 0.1,
#'    sigma_H2O_r = 0.1
#'  )
#'    
#' ph = photosynthesis(lp, ep, bp, cs, use_tealeaves = FALSE, quiet = TRUE) |>
#'   simulate_error(chamber_pars, n = 1L)
#'   
#' @md
#' @export
simulate_error = function(
    ph_out,
    chamber_pars,
    n = 1L,
    use_tealeaves = ("T_air" %in% colnames(ph_out))
  ) {
  
  lifecycle::signal_stage("experimental", what = "simulate_error()")
  
  # Check
  checkmate::assert_data_frame(ph_out, any.missing = FALSE, min.rows = 1L)
  c("A", "C_i", "g_sc", "P", "T_leaf") %in% colnames(ph_out) |>
    all() |>
    checkmate::assert_true()
  checkmate::assert_data_frame(chamber_pars, any.missing = FALSE, nrows = 1L)
  c("flow", "leaf_area", "sigma_CO2_r", "sigma_CO2_s", "sigma_H2O_r", "sigma_H2O_s") |>
    magrittr::is_in(colnames(chamber_pars)) |>
    all() |>
    checkmate::assert_true()
  checkmate::assert_number(chamber_pars$flow, lower = 0, finite = TRUE)
  checkmate::assert_number(chamber_pars$leaf_area, lower = 0, finite = TRUE)
  checkmate::assert_number(chamber_pars$sigma_CO2_r, lower = 0, finite = TRUE)
  checkmate::assert_number(chamber_pars$sigma_CO2_s, lower = 0, finite = TRUE)
  checkmate::assert_number(chamber_pars$sigma_H2O_r, lower = 0, finite = TRUE)
  checkmate::assert_number(chamber_pars$sigma_H2O_s, lower = 0, finite = TRUE)
  checkmate::assert_int(n)
  checkmate::assert_flag(use_tealeaves)
  
  # Replicate ph_out n times and add chamber parameters
  tidyr::crossing(
    .rep = seq_len(n),
    ph_out,
    chamber_pars
  ) %>%
    dplyr::mutate(
      
      # Assume water vapour concentration is saturated within leaf, w_i [mmol/mol]
      w_i = if (use_tealeaves) {
    # Use tealeaves version for internal consistency
    tealeaves:::.get_ps(T_leaf, P, FALSE) |>
      magrittr::divide_by(P) |>
      set_units(mmol/mol)
  } else {
    # LI-6800 equation
    T_leaf = set_units(T_leaf, degreeC) |>
      drop_units()
    P = set_units(P, kPa) |>
      drop_units()
    w_i = set_units((1000 * 0.61365 * exp(17.502 * T_leaf / (240.97 + T_leaf)) / P), mmol / mol)
  },
  # Calculate [H2O] in chamber based on RH
  w_a = set_units(.data$RH * w_i, mmol / mol),
  # Assume all g_tw = g_sw (i.e. g_uw = 0; g_bw = Inf
  g_tw = .data$g_sc * 1.6,
  E_area = set_units(.data$g_tw * (w_i - .data$w_a), mmol / m^2 / s),
  E = .data$E_area * .data$leaf_area, # [mmol / s]
  w_0 = set_units(.data$w_a - .data$E * (set_units(1) - .data$w_a) / .data$flow,
                  mmol / mol),
  c_0 = set_units(.data$leaf_area * .data$A / .data$flow + 
                    .data$C_air * (set_units(1) - .data$w_0) / 
                    (set_units(1) - .data$w_a), umol / mol),
  
  # Simulate measurements with error
  H2O_s = .data$w_a + set_units(rnorm(nrow(.), 0, .data$sigma_H2O_s), 
                                mmol / mol),
  H2O_r = .data$w_0 + set_units(rnorm(nrow(.), 0, .data$sigma_H2O_r), 
                                mmol / mol),
  CO2_s = .data$C_air + set_units(rnorm(nrow(.), 0, .data$sigma_CO2_s), 
                                  umol / mol),
  CO2_r = .data$c_0 + set_units(rnorm(nrow(.), 0, .data$sigma_CO2_r), 
                                umol / mol),
  
  # Derived estimates with error
  E_meas = set_units(.data$flow * (.data$H2O_s - .data$H2O_r) / 
                       (set_units(1) - .data$H2O_s), mmol/s),
  E_area_meas = set_units(.data$E_meas / .data$leaf_area, mmol / m^2 / s),
  g_tw_meas = .data$E_area_meas * (set_units(1) - (.data$w_i + .data$H2O_s) / 2) / (.data$w_i - .data$H2O_s),
  g_sc_meas = .data$g_tw_meas / 1.6, # this is only valid if cuticular conductance is 0 and boundary layer conductance is Inf
  g_tc_meas = .data$g_sc_meas, # valid above assumption is true and g_mc is Inf
  A_meas = set_units(.data$flow * (.data$CO2_r - .data$CO2_s * ((set_units(1) - .data$H2O_r) / (set_units(1) - .data$H2O_s))) / .data$leaf_area, umol / m^2 / s),
  C_i_meas = set_units(((.data$g_tc_meas - .data$E_area_meas / 2) * .data$CO2_s - .data$A_meas) / (.data$g_tc_meas + .data$E_area_meas / 2), umol/mol)
  
    ) 
  
}
