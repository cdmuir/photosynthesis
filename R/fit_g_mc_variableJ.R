#' Fitting mesophyll conductance with the variable J method
#'
#' @param data Dataframe
#' @param varnames Variable names to fit g_mc. varnames = list(A_net = "A_net",
#' J_etr = "J_etr", C_i = "C_i", PPFD = "PPFD", phi_PSII = "phi_PSII"), where
#' A_net is net CO2 assimilation in umol m-2 s-1, J_etr is linear electron
#' transport flux in umol m-2 s-1, C_i is intercellular CO2 concentration in
#' umol mol-1, PPFD is incident irradiance in umol m-2 s-1, phi_PSII is
#' the operating efficiency of photosystem II.
#' @param usealpha_Q Recalculate electron transport with new absorbance value?
#' @param alpha_Q Absorbance of photosynthetically active radiation
#' @param beta_Q Partitioning of absorbed light energy between PSI and PSII
#' @param gamma_star Photorespiratory CO2 compensation point in umol mol-1
#' @param R_d Respiration rate in umol m-2 s-1
#' @param P Atmospheric pressure in kPa
#'
#' @return fit_g_mc_variableJ fits mesophyll conductance according
#' to Harley et al. 1992. It also tests the reliability of the
#' calculation and calculates a mean with only reliable values.
#' Note that the output is in units of umol m-2 s-1 Pa-1.
#'
#' @references
#' Harley PC, Loreto F, Di Marco G, Sharkey TD. 1992. Theoretical
#' considerations when estimating mesophyll conductance to CO2 flux
#' by analysis of the response of photosynthesis to CO2. Plant Physiol
#' 98:1429 - 1436.
#' @export
#'
#' @examples
#' \donttest{
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Note: there will be issues here if the alpha value used
#' # for calculating ETR is off, if gamma_star is incorrect,
#' # if R_d is incorrect.
#' data <- fit_g_mc_variableJ(data,
#'   varnames = list(
#'     A_net = "A",
#'     J_etr = "ETR",
#'     C_i = "Ci",
#'     PPFD = "Qin",
#'     phi_PSII = "PhiPS2"
#'   ),
#'   gamma_star = 46,
#'   R_d = 0.153,
#'   usealpha_Q = TRUE,
#'   alpha_Q = 0.84,
#'   beta_Q = 0.5,
#'   P = 84
#' )
#'
#' # Note that many g_mc values from this method can be unreliable
#' ggplot(data, aes(x = CO2_s, y = g_mc, colour = reliable)) +
#'   labs(
#'     x = expression(CO[2] ~ "(" * mu * mol ~ mol^
#'       {
#'         -1
#'       } * ")"),
#'     y = expression(g[m] ~ "(mol" ~ m^{
#'       -2
#'     } ~ s^{
#'       -1
#'     } ~ Pa^
#'       {
#'         -1
#'       } * ")")
#'   ) +
#'   geom_point(size = 2) +
#'   theme_bw() +
#'   theme(legend.position = "bottom")
#'
#' # Plot QAQC graph according to Harley et al. 1992
#' ggplot(data, aes(x = CO2_s, y = dCcdA, colour = reliable)) +
#'   labs(
#'     x = expression(CO[2] ~ "(" * mu * mol ~ mol^
#'       {
#'         -1
#'       } * ")"),
#'     y = expression(delta * C[chl] * "/" * delta * A)
#'   ) +
#'   geom_hline(yintercept = 10) +
#'   geom_point(size = 2) +
#'   theme_bw() +
#'   theme(legend.position = "bottom")
#' }
fit_g_mc_variableJ <- function(data,
                               varnames = list(
                                 A_net = "A_net",
                                 J_etr = "J_etr",
                                 C_i = "C_i",
                                 PPFD = "PPFD",
                                 phi_PSII = "phi_PSII"
                               ),
                               usealpha_Q = FALSE,
                               alpha_Q = 0.84,
                               beta_Q = 0.5,
                               gamma_star,
                               R_d,
                               P = 100) {
  # Set variable names in data if different from defaults
  data$A_net <- data[, varnames$A_net]
  data$J_etr <- data[, varnames$J_etr]
  data$C_i <- data[, varnames$C_i] # In umol / mol
  # If assigning alpha_Q, re-calculate J_etr, otherwise use J_etr
  if (usealpha_Q) {
    data$PPFD <- data[, varnames$PPFD]
    data$phi_PSII <- data[, varnames$phi_PSII]
    data$J_etr <- data$PPFD * alpha_Q * beta_Q * data$phi_PSII
  } else {
    data$J_etr <- data[, varnames$J_etr]
  }
  # Convert C_i and gamma_star into Pa
  data$C_i_pa <- data$C_i / 1000000 * P * 1000
  gamma_star <- gamma_star / 1000000 * P * 1000
  # Calculate g_mc according to Harley et al. 1992
  data$g_mc <- data$A_net /
    (data$C_i_pa - (gamma_star * (data$J_etr + 8 * (data$A_net + R_d)) /
      (data$J_etr - 4 * (data$A_net + R_d))))

  # According to Harley et al. 1992, if dCc/dA_net is too great
  # g_mc is super sensitive to small errors, while if dCc/dA is too
  # small, the results can be "unbelieveable". They suggested a
  # range of 10 to 50 being acceptable, so the calculations for
  # reliability are based on those. Please note that they may
  # since have been mistaken, so this range is more of a guide
  # rather than a hard and fast rule.
  data$dCcdA <- 12 * gamma_star * data$J_etr /
    (data$J_etr - 4 * (data$A_net + R_d))^2

  data$reliable_g_mc <- rep(TRUE, length(data$g_mc))
  # for loop to add TRUE/FALSE values to the reliability of the
  # g_mc measurements. Cutoffs based on Harley et al. 1992.
  for (i in 1:length(data$reliable_g_mc)) {
    if (data$dCcdA[i] < 10) {
      data$reliable[i] <- FALSE
    }
    if (data$dCcdA[i] > 50) {
      data$reliable[i] <- FALSE
    }
  }
  # Calculate the mean of reliable g_mc values
  data$mean_g_mc_reliable <- mean(data[data$reliable == TRUE, ]$g_mc)
  # return allows the output to be stored in the global environment
  return(data)
}
