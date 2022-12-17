#' Simulate C3 photosynthesis in two dimensions using a porous medium approximation
#' @export
#' 
photo_2d_porousmedium = function(
    leaf_par, 
    enviro_par, 
    bake_par, 
    constants,
    quiet = FALSE, 
    assert_units = TRUE,
    check = TRUE
) {

  checkmate::assert_flag(check)
  
  # Check arguments ----
  if (check) {
    # more checks
    checkmate::assert_flag(quiet)
    checkmate::assert_flag(quiet)
  }
  
  # Assert units ----
  if (assert_units) {
    
  }
  
  # Convert K_C and K_O from partial pressure (mol/mol) to volumetric (mol/m^3)
  # Henry's Law: Solubility = constant * partial pressure in air
  # Von't Hoff equation to describe temperature dependence of constant
  # Use von Caemmerer (2000) method.
  # For CO2: solubility is 0.0334 mol / L / bar * bar / 100 kPa = 0.000334 mol / L / kPa = 0.334 mol/m^3/kPa
  # For O2: solubility is 0.00126 mol / L / bar * bar / 100 kPa = 0.000126 mol / L / kPa = 0.126 mol/m^3/kPa
  # Based on Data from NIST Standard Reference Database 69: NIST Chemistry WebBook
#https://webbook.nist.gov/cgi/cbook.cgi?ID=C124389&Mask=10
  kHc25 = set_units(3.3e-4, mol/m^3/Pa)
  dkHc = set_units(2400, K)
  T_K = set_units(298.15, K)
  k_Hc = .get_kHc(kHc25, dkHc, T_K, FALSE)
  
  K_m = pars$K_C * (set_units(1) + pars$O / pars$K_O)
  K_m_vol = set_units(K_m * k_Hc * pars$P, mol/ m^3) # THIS IS WHAT I WANT
  
  # Convert gamma_star from umol/mol to mol / m ^ 3 using ideal gas law
  set_units(pars$gamma_star * set_units(pars$R * pars$T_leaf / pars$P, mol / m^3), mol/m^3)
  
  # ELEMENT-WISE PARAMETERS
  # Chlorophyll per volume as a function of depth
  # Use approach from Borsuk and Brodersen (2019). Coefficients are "typical" leaf
  rel_chlorophyll_percent = function(
    depth_percent, 
    b0_relchl = 67.52,
    b1_relchl = 0.4149,
    b2_relchl = -0.0029
  ) {
    b0_relchl + b1_relchl * depth_percent + b2_relchl * depth_percent ^ 2
  }
  x = seq(0, 100, 1)
  y= rel_chlorophyll_percent(x)
  # convert relative chl to absolute by taking chl/m^2 leaf -> chl / m^3 locally
  # function to convert local rate of photon absorption per Chl (mol photon / mol Chl / s). This is in eq 10 of Gutschick 1984.
  plot(x, y)
  # Convert dark respiration to volumetric units
  # Rd_vol is the volumetric dark respiration rate for mitochondria + stroma
  # See explanation in Earles et al. (2017), pg. 1094
  # SHOULD I MAKE THIS SO R_d25 IS SPECIFIED ON AREA BASIS AND VOLUMETRIC CALCULATED? THIS WHAT I DO FOR J
  pars$r_d25 = set_units(0.066, mol/m^3/s)
  
  # Volumetric electron transport rate
  # See Equation 5 of Tholen and Zhu (2011)
  # Wait - seems like better way is to have local j calculated from local
  pars$J_max
  
}

#' Internal functions to help simulate C3 photosynthesis in two dimensions using 
#' a porous medium approximation
#' 
#' * \code{\link{.get_kHc}}: Calculate the Henryʻs Law constant for the solubility of CO2 in water as a function of temperature
#' 
#' @rdname photosynthesis-2d-porousmedium
#' @md
NULL

#' Calculate the Henryʻs Law constant for the solubility of CO2 in water as a function of temperature
#' 
#' @inheritParams A_supply
#' @param kHc25 Henry's Law constant for the solubility of CO2 in water
#' @param dkHc Coefficient describing the temperature dependence of Henryʻs Law constant for the solubility of CO2 in water [K]
#' @param T_K Temperature [K]
#' 
#' @details Henry's Law coefficient for the solubility of CO\eqn{_2} in water at 298.15 K is 0.034 mol/L/atm (Sander 2015). We use the van't Hoff equation to calculate the constant as a function of temperature where \eqn{d_{k\mathrm{Hc}}{d_kHc} = 2400 K (Sander 2015).
#' 
#' \deqn{k_{\mathrm{Hc}} = k_{\mathrm{Hc,25}} e^{d_{k\mathrm{Hc}} (1 / T_K - 1 / 298.15)}}
#' 
#' @references 
#' Sander, R. 2015. Compilation of Henry's law constants (version 4.0) for water 
#' as solvent. Atmos. Chem. Phys. 15: 4399–4981. https://doi.org/10.5194/acp-15-4399-2015
#' 
#' @md
.get_kHc = function(kHc25, dkHc, T_K, unitless) {
  log_d = if (unitless) {
    dkHc * (1 / T_K - 1 / 298.15)
  } else {
    drop_units(dkHc * ((1 / T_K) - 1 / set_units(298.15, K)))
  }
  kHc25 * exp(log_d)
} 

# notes on where default values come from
# Sm_palisade (Earles et al. 2017)
# V_m (Tholen and Zhu 2011)