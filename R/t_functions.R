#' Temperature response functions
#'
#' @param T_leaf Leaf temperature in K
#'
#' @param a Constant to minimize residuals (Heskel et al. 2016)
#' @param b Linear coefficient to minimize residuals (Heskel et al. 2016)
#' @param c Quadratic coefficient to minimize residuals (Heskel et al. 2016)
#'
#' @param T2 Leaf temperature term (Kruse et al. 2008)
#' @param dEa Temperature-dependent change in Ea in K^2 (Kruse et al. 2008)
#' @param Ea_ref Activation energy in J mol-1 (Kruse et al. 2008)
#' @param Par_ref Parameter at reference temperature of 25 Celsius (Kruse et
#' al. 2008)
#'
#' @param dS Entropy parameter in J mol-1 (Medlyn et al. 2002)
#' @param Ea Activation energy in J mol-1 (Medlyn et al. 2002)
#' @param Hd Deactivation energy in J mol-1 (Medlyn et al. 2002)
#' @param Topt Optimum temperature of the process in K (Medlyn et al.
#' 2002)
#'
#' @param dH Change in enthalpy of the reaction at 25 C in J mol-1 (Hobbs et
#' al. 2013)
#' @param dCp Change in heat capacity of the enzyme between the
#' enzyme-substrate #' and enzyme-transition states in J mol-1 K-1 (Hobbs et
#' al. 2013)
#' @param dG Change in Gibbs free energy of the reaction at 25 C in J mol-1
#' (Hobbs et al. 2013)
#'
#' @return t_response_arrhenius calculates the rate of a process based on an
#' Arrhenius-type curve
#'
#' t_response_arrhenius_kruse fits a peaked Arrhenius response according to
#' Kruse et al. 2008.
#'
#' t_response_arrhenius_medlyn is a peaked Arrhenius response as found in
#' Medlyn et al. 2002.
#'
#' t_response_arrhenius_topt is a peaked Arrhenius temperature response
#' function.
#'
#' t_response_calc_dS calculates dS from the fitted Topt model.
#'
#' t_response_calc_topt calculates Topt for a process from Arrhenius
#' parameters.
#'
#' t_response_heskel is a quadratic temperature response according to
#' Heskel et al. 2016.
#'
#' t_response_mmrt is a macromolecular rate theory temperature response
#' according to Hobbs et al. 2013.
#'
#' @references
#'
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#'
#' Heskel et al. 2016. Convergence in the temperature response of leaf
#' respiration across biomes and plant functional types. PNAS 113:3832-3837
#'
#' Hobbs et al. 2013. Change in heat capacity for enzyme catalysis
#' determines temperature dependence of enzyme catalyzed rates. ACS Chemical
#' Biology 8:2388-2393
#'
#' Kruse J, Adams MA. 2008. Three parameters comprehensively describe
#' the temperature response of respiratory oxygen reduction. Plant
#' Cell Environ 31:954-967
#'
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#'
#' @rdname t_functions
#' @export
t_response_arrhenius <- function(T_leaf, Ea) {
  exp(Ea * ((T_leaf) - 298.15) /
    (298.15 * 8.314 * (T_leaf)))
}

#' @rdname t_functions
#' @export
t_response_arrhenius_kruse <- function(dEa, Ea_ref, Par_ref, T2) {
  log(Par_ref) + (Ea_ref / 8.314) * T2 + dEa * T2^2
}

#' @rdname t_functions
#' @export
t_response_arrhenius_medlyn <- function(T_leaf, Ea, Hd, dS) {
  exp(Ea * ((T_leaf) - 298.15) /
    (298.15 * 8.314 * (T_leaf))) *
    (1 + exp((298.15 * dS - Hd) / (298.15 * 8.314))) /
    (1 + exp(((T_leaf) * dS - Hd) / ((T_leaf) * 8.314)))
}

#' @rdname t_functions
#' @export
t_response_arrhenius_topt <- function(T_leaf, Ea, Hd, Topt) {
  Hd * exp(Ea * ((T_leaf) - (Topt)) /
    ((T_leaf) * (Topt) * 8.314)) /
    (Hd - Ea * (1 - exp(Hd * ((T_leaf) - (Topt)) /
      ((T_leaf) * (Topt) * 8.314))))
}

#' @rdname t_functions
#' @export
t_response_calc_dS <- function(Ea,
                               Hd,
                               Topt) {
  Hd / Topt + 8.314 * log(Ea / (Hd - Ea))
}

#' @rdname t_functions
#' @export
t_response_calc_topt <- function(Hd, dS, Ea) {
  Hd / (dS - 8.314 * log(Ea / (Hd - Ea)))
}

#' @rdname t_functions
#' @export
t_response_heskel <- function(T_leaf, a, b, c) {
  a + b * (T_leaf - 273.15) + c * (T_leaf - 273.15)^2
}

#' @rdname t_functions
#' @export
t_response_mmrt <- function(dCp,
                            dG,
                            dH,
                            T_leaf) {
  (log(1.380649e-23 * (298.15) / 6.62607e-34)) -
    dG / (8.314 * 298.15) +
    (1 / 298.15 + dH / (8.314 * 298.15^2)) * ((T_leaf) - 298.15) +
    (dCp / (2 * 8.314 * 298.15^2)) * ((T_leaf) - 298.15)^2
}
