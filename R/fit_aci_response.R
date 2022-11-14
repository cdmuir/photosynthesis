#' Fitting ACi curves
#'
#' @param data Dataframe for A-Ci curve fitting
#' @param varnames List of variable names. varnames = list(A_net = "A_net",
#' T_leaf = "T_leaf", C_i = "C_i", PPFD = "PPFD", g_mc = "g_mc"), where A_net
#' is net CO2 assimilation, T_leaf is leaf temperature in Celsius, C_i is
#' intercellular CO2 concentration in umol/mol, PPFD is incident irradiance
#' in umol m-2 s-1 (note that it is ASSUMED to be absorbed irradiance, so be
#' sure to adjust according to light absorbance and PSI/PSII partitioning
#' accordingly OR interpret the resultant values of J and J_max with caution),
#' g_mc is mesophyll conductance to CO2 in mol m-2 s-1 Pa-1.
#' @param P Atmospheric pressure in kPa
#' @param fitTPU Should triose phosphate utilization (V_TPU) be fit?
#' @param alpha_g Fraction of respiratory glycolate carbon that is not returned
#' to the chloroplast (von Caemmerer, 2000). If ACi curves show high-CO2
#' decline, then this value should be > 0.
#' @param R_d_meas Measured value of respiratory CO2 efflux in umol m-2 s-1.
#' Input value should be positive to work as expected with the equations.
#' @param useR_d Use a measured value of R_d? Set to TRUE if using R_d_meas.
#' @param useg_mc Use mesophyll conductance? Set to TRUE if specifying g_mc
#' in varnames above.
#' @param useg_mct Use mesophyll conductance temperature response? Set to TRUE
#' if using a temperature response of mesophyll conductance.
#' @param usegamma_star Specify gamma_star value? If FALSE, uses a temperature
#' response function with Nicotiana tabacum defaults from Bernacchi et al.
#' 2001.
#' @param useK_M Specify K_M? If FALSE, uses an Arrhenius temperature response
#' function with Nicotiana tabacum defaults from Bernacchi et al. 2001.
#' @param useK_C_K_O Use individual carboxylation/oxygenation constants for
#' rubisco? If TRUE, need to specify values at 25C and activation energy for
#' the Arrhenius temperature response function.
#' @param alpha Quantum yield of CO2 assimilation
#' @param theta_J Curvature of the photosynthetic light response curve
#' @param gamma_star25 gamma_star at 25C in umol mol-1
#' @param Ea_gamma_star Activation energy of gamma_star in J mol-1
#' @param K_M25 Michaelis-Menten constant for rubisco at 25C
#' @param Ea_K_M Activation energy for K_M in J mol-1
#' @param g_mc25 Mesophyll conductance at 25C in mol m-2 s-1
#' @param Ea_g_mc Activation energy of g_mc in J mol-1
#' @param K_C25 Michaelis-Menten constant for rubisco carboxylation at 25C
#' @param Ea_K_C Activation energy for K_C in J mol-1
#' @param K_O25 Michaelis-Menten constant for rubisco oxygenation at 25C
#' @param Ea_K_O Activation energy for K_O in J mol-2
#' @param Oconc O2 concentration in %. Used with P to calculate
#' intracellular O2 when using K_C_K_O
#' @param gamma_star_set Value of gamma_star to use (in ppm) if
#' usegamma_star = TRUE
#' @param K_M_set Value of K_M to use if useK_M = TRUE
#' @param ... Other arguments to pass on
#'
#' @return fit_aci_response fits ACi curves using an approach similar to
#' Gu et al. 2010. Iterates all possible C_i transition points and checks for
#' inadmissible curve fits. If no curves are admissible (either due to poor data
#' or poor assumed parameters), the output will include a dataframe of NA values.
#' Default parameters are all from Bernacchi et al. 2001, 2002.
#'
#' @references
#' Bernacchi CJ, Singsaas EL, Pimentel C, Portis AR, Long SP. 2001. Improved
#' temperature response functions for models of rubisco-limited photosynthesis.
#' Plant Cell Environment 24:253-259.
#'
#' Bernacchi CJ, Portis AR, Nakano H, von Caemmerer S, Long SP. 2002. Temperature
#' response of mesophyll conductance. Implications for the determination of rubisco
#' enzyme kinetics and for limitations to photosynthesis in vivo. Plant Physiology
#' 130:1992-1998.
#'
#' Gu L, Pallardy SG, Tu K, Law BE, Wullschleger SD. 2010. Reliable estimation
#' of biochemical parameters from C3 leaf photosynthesis-intercellular carbon
#' dioxide response curves. Plant Cell Environment 33:1852-1874.
#'
#' von Caemmerer S. 2000. Biochemical models of leaf photosynthesis. CSIRO
#' Publishing, Collingwood.
#'
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom stats coef
#' @importFrom stats lm
#' @importFrom stats sd
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
#' # Define a grouping factor based on light intensity to split the ACi
#' # curves
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#'
#' # Convert leaf temperature to K
#' data$T_leaf <- data$Tleaf + 273.15
#'
#' # Fit ACi curve. Note that we are subsetting the dataframe
#' # here to fit for a single value of Q_2
#' fit <- fit_aci_response(data[data$Q_2 == 1500, ],
#'   varnames = list(
#'     A_net = "A",
#'     T_leaf = "T_leaf",
#'     C_i = "Ci",
#'     PPFD = "Qin"
#'   )
#' )
#'
#' # View fitted parameters
#' fit[[1]]
#'
#' # View graph
#' fit[[2]]
#'
#' # View data with modelled parameters attached
#' fit[[3]]
#'
#' # Fit many curves
#' fits <- fit_many(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     T_leaf = "T_leaf",
#'     C_i = "Ci",
#'     PPFD = "Qin"
#'   ),
#'   funct = fit_aci_response,
#'   group = "Q_2"
#' )
#'
#' # Print the parameters
#' # First set of double parentheses selects an individual group value
#' # Second set selects an element of the sublist
#' fits[[3]][[1]]
#'
#' # Print the graph
#' fits[[3]][[2]]
#'
#' # Compile graphs into a list for plotting
#' fits_graphs <- compile_data(fits,
#'   list_element = 2
#' )
#'
#' # Compile parameters into dataframe for analysis
#' fits_pars <- compile_data(fits,
#'   output_type = "dataframe",
#'   list_element = 1
#' )
#' }
fit_aci_response <- function(data,
                             varnames = list(
                               A_net = "A_net",
                               T_leaf = "T_leaf",
                               C_i = "C_i",
                               PPFD = "PPFD",
                               g_mc = "g_mc"
                             ),
                             P = 100,
                             fitTPU = TRUE,
                             alpha_g = 0,
                             R_d_meas = NULL,
                             useR_d = FALSE,
                             useg_mc = FALSE,
                             useg_mct = FALSE,
                             usegamma_star = FALSE,
                             useK_M = FALSE,
                             useK_C_K_O = FALSE,
                             alpha = 0.24,
                             theta_J = 0.85,
                             gamma_star25 = 42.75,
                             Ea_gamma_star = 37830,
                             K_M25 = 718.40,
                             Ea_K_M = 65508.28,
                             g_mc25 = 0.08701,
                             Ea_g_mc = 0,
                             K_C25 = NULL,
                             Ea_K_C = NULL,
                             K_O25 = NULL,
                             Ea_K_O = NULL,
                             Oconc = 21,
                             gamma_star_set = NULL,
                             K_M_set = NULL,
                             ...) {
  # Locally bind variables - avoids notes on check package
  C_i <- NULL
  A_model <- NULL
  A_carbox <- NULL
  A_regen <- NULL
  A_tpu <- NULL
  A_net <- NULL
  PPFD <- NULL
  # Set variable names
  data$C_i <- data[, varnames$C_i]
  data$A_net <- data[, varnames$A_net]
  data$PPFD <- data[, varnames$PPFD]
  data$T_leaf <- data[, varnames$T_leaf]
  outputs <- vector("list", 3)
  # Order data by increasing C_i, avoids calculation issues
  data <- data[order(data$C_i), ]
  # Convert O2 concentration to partial pressure
  O <- Oconc * P / 100
  # Create grid of possible C_i transition points
  ci <- data[order(data$C_i), ]$C_i
  nci <- length(ci)
  citransitions <- diff(ci) / 2 + ci[-nci]
  # Make sure there is a minimum of 3 points for V_cmax fitting
  citransitions1 <- citransitions[3:length(citransitions)]
  if (!fitTPU) {
    citransitions2 <- max(ci) + 1
  } else {
    citransitions2 <- c(max(ci) + 1, rev(citransitions1))
  }
  # Create combinations of ci1 and ci2 to fit
  citransdf <- expand.grid(ci1 = citransitions1, ci2 = citransitions2)
  citransdf <- citransdf[citransdf$ci1 <= citransdf$ci2, ]
  # Mesophyll conductance calculations
  if (!useg_mc) {
    # Assumes g_mc is infinite
    data$C <- data$C_i * P / 100
  } else {
    # Uses measured values of g_mc
    data$g_mc <- data[, varnames$g_mc]
    data$C <- (data$C_i - data$A_net / data$g_mc) * P / 100
  }
  if (useg_mct) {
    # Calculates g_mc based on a specified temperature response
    data$g_mc <- g_mc25 * t_response_arrhenius(
      T_leaf = data$T_leaf,
      Ea = Ea_g_mc
    )
    data$C <- (data$C_i - data$A_net / data$g_mc) * P / 100
  }
  # gamma_star settings
  if (!usegamma_star) {
    # Calculates gamma_star based on temperature response function
    gamma_star <- gamma_star25 * t_response_arrhenius(
      T_leaf = mean(data$T_leaf),
      Ea = Ea_gamma_star
    ) * P / 100
  } else {
    # Uses specified gamma_star, converts to partial pressure
    gamma_star <- gamma_star_set * P / 100
  }
  # K_M settings
  if (!useK_M) {
    # Calculates K_M based on temperature response
    K_M <- K_M25 * t_response_arrhenius(
      T_leaf = mean(data$T_leaf),
      Ea = Ea_K_M
    )
  } else {
    # Uses specified K_M
    K_M <- K_M_set
  }
  if (useK_C_K_O) {
    # Calculates K_M based on temperature responses of K_C and K_O
    K_C <- K_C25 * t_response_arrhenius(
      T_leaf = mean(data$T_leaf),
      Ea = Ea_K_C
    )
    K_O <- K_O25 * t_response_arrhenius(
      T_leaf = mean(data$T_leaf),
      Ea = Ea_K_O
    )
    K_M <- K_C * (1 + O / K_O)
  }
  # Generate x-variables for linearized prediction of V_cmax, J_max, V_TPU
  # Note this is based on the Duursma (2015) approach eto Gu et al. 2010
  data$V_cmax_pred <- (data$C - gamma_star) / (data$C + K_M)
  data$J_max_pred <- (data$C - gamma_star) / (data$C + 2 * gamma_star)
  data$V_TPU_part <- (data$C - gamma_star) / (data$C - (1 + 3 * alpha_g) *
    gamma_star)
  # Create dataframe for all possible curve fits
  poss_fits <- data.frame(matrix(0,
    nrow = nrow(citransdf),
    ncol = 16
  ))
  # Add column names
  colnames(poss_fits) <- c(
    "Num", "V_cmax", "V_cmax_se", "J_max",
    "J", "J_se", "V_TPU", "V_TPU_se", "R_d", "R_d_se",
    "cost", "citransition1", "citransition2",
    "V_cmax_pts", "J_max_pts", "V_TPU_pts"
  )
  # Fit all possible citransition combinations
  for (i in seq_len(nrow(citransdf))) {
    # Locally bind variables
    cost <- NULL
    V_cmax_fit <- NULL
    J_max_fit <- NULL
    V_TPU <- NULL
    datc <- NULL
    datj <- NULL
    datp <- NULL
    datcomp <- NULL
    # CO2-limited points
    datc <- data[data$C_i < citransdf$ci1[i], ]
    # RuBP regeneration-limited points
    datj <- data[data$C_i > citransdf$ci1[i] &
      data$C_i < citransdf$ci2[i], ]
    # V_TPU-limited points
    datp <- data[data$C_i > citransdf$ci2[i], ]
    # Fits V_cmax
    if (!useR_d) {
      # Fit R_d and V_cmax
      fitc <- lm(A_net ~ V_cmax_pred, data = datc)
      R_d_fit <- coef(fitc)[[1]]
      R_d_se <- summary(fitc)$coefficients[1, 2]
      V_cmax_fit <- coef(fitc)[[2]]
      V_cmax_se <- summary(fitc)$coefficients[2, 2]
      datc$A_gross <- datc$A_net - R_d_fit
    } else {
      # Use R_d and fit V_cmax
      R_d_fit <- -R_d_meas
      datc$A_gross <- datc$A_net - R_d_fit
      fitc <- lm(A_gross ~ V_cmax_pred - 1, data = datc)
      V_cmax_fit <- coef(fitc)[[1]]
      V_cmax_se <- summary(fitc)$coefficients[, 2]
    }
    # Fit J and J_max
    if (nrow(datj) > 0) {
      datj$A_gross <- datj$A_net - R_d_fit
      if (nrow(datj) == 1) {
        # Calculates J_max based on one point
        J_fit <- 4 * datj$A_gross / datj$J_max_pred
        J_se <- NA
        J_max_fit <- suppressWarnings(calculate_jmax(mean(data$PPFD),
          alpha,
          J = J_fit, theta_J
        ))
      } else {
        # Calculates J_max based on a linear regression fit
        fitj <- lm(A_gross ~ J_max_pred - 1, data = datj)
        J_fit <- 4 * coef(fitj)[[1]]
        J_se <- summary(fitj)$coefficients[2]
        J_max_fit <- suppressWarnings(calculate_jmax(mean(data$PPFD),
          alpha,
          J = J_fit, theta_J
        ))
      }
    } else {
      # Assign J_max a value of 10 ^ 6 if there's no RuBP limitation
      J_max_fit <- 10^6
      J_max_SS <- 0
      J_se <- NA
    }
    # Calculating V_TPU limitations
    if (nrow(datp) == 1 && nrow(datj) == 0) {
      # Assign V_TPU a value of 1000 if there is only 1 assigned
      # point and no RuBP-limited points
      datp$A_gross <- datp$A_net - R_d_fit
      V_TPU <- 1000
      V_TPU_SS <- 0
      V_TPU_se <- NA
    } else {
      # This section covers if there are no V_TPU-limited points
      datp$A_gross <- datp$A_net - R_d_fit
      V_TPU <- 1000 # same as default in Photosyn
      V_TPU_SS <- 0
      V_TPU_se <- NA
    }
    # Calculates V_TPU limitations if there are at least 3 points
    # to ensure more reliable fit
    if (nrow(datp) > 2) {
      datp$A_gross <- datp$A_net - R_d_fit
      V_TPU_vals <- (1 / 3) * datp$A_gross / datp$V_TPU_part
      V_TPU <- mean(V_TPU_vals)
      V_TPU_se <- sd(V_TPU_vals) / sqrt(length(V_TPU_vals))
    }
    # If V_TPU is fit to be < 0, assign value of 1000. Avoids
    # strange issues.
    if (V_TPU < 0) {
      V_TPU <- 1000
      V_TPU_SS <- 0
      V_TPU_se <- NA
    }
    # Calculate rates of photosynthesis for each limitation state and sums of
    # squares for model-wise cost function
    # CO2 limitations
    datc$A <- V_cmax_fit * (datc$C - gamma_star) / (datc$C + K_M) + R_d_fit
    datc$SS <- (datc$A - datc$A_net)^2
    V_cmax_SS <- sum(datc$SS)
    # RuBP limitations
    datj$A <- J_fit * (datj$C - gamma_star) /
      (4 * datj$C + 8 * gamma_star) + R_d_fit
    datj$SS <- (datj$A - datj$A_net)^2
    J_max_SS <- sum(datj$SS)
    # V_TPU limitations
    datp$A <- 3 * V_TPU * (datp$C - gamma_star) /
      (datp$C - (1 + 3 * alpha_g) * gamma_star / datp$C) + R_d_fit
    datp$SS <- (datp$A - datp$A_net)^2
    V_TPU_SS <- sum(datp$SS)
    # Calculate cost functions
    cost <- 1 / 2 * (V_cmax_SS + J_max_SS + V_TPU_SS)
    # Bind calculated data
    datcomp <- rbind(datc, datj, datp)
    # Add outputs to possible fits
    poss_fits$V_cmax[i] <- V_cmax_fit
    poss_fits$V_cmax_se[i] <- V_cmax_se
    poss_fits$J_max[i] <- J_max_fit
    poss_fits$J[i] <- J_fit
    poss_fits$J_se[i] <- J_se
    poss_fits$V_TPU[i] <- V_TPU
    poss_fits$V_TPU_se[i] <- V_TPU_se
    poss_fits$R_d[i] <- R_d_fit
    poss_fits$R_d_se[i] <- R_d_se
    poss_fits$V_cmax_pts[i] <- nrow(datc)
    poss_fits$J_max_pts[i] <- nrow(datj)
    poss_fits$V_TPU_pts[i] <- nrow(datp)
    poss_fits$cost[i] <- cost
    poss_fits$citransition1[i] <- citransdf$ci1[i]
    poss_fits$citransition2[i] <- citransdf$ci2[i]
  } # End curve fitting of all possible C_i transitions
  # Select fit with minimized cost function
  best_fits <- poss_fits[poss_fits$cost == min(poss_fits$cost), ]
  # New segment for sensitivity analysis
  # Adds values for assumed constants to the output dataframe
  best_fits$alpha <- alpha
  best_fits$alpha_g <- alpha_g
  best_fits$gamma_star25 <- gamma_star25
  best_fits$Ea_gamma_star <- Ea_gamma_star
  best_fits$K_M25 <- K_M25
  best_fits$Ea_K_M <- Ea_K_M
  best_fits$g_mc25 <- g_mc25
  best_fits$Ea_g_mc <- Ea_g_mc
  best_fits$K_C25 <- K_C25
  best_fits$Ea_K_C <- Ea_K_C
  best_fits$K_O25 <- K_O25
  best_fits$Ea_K_O <- Ea_K_O
  best_fits$Oconc <- Oconc
  best_fits$theta_J <- theta_J
  # Calculate net photosynthetic rates
  data$A_carbox <- best_fits$V_cmax * data$C /
    (data$C + K_M) * (1 - gamma_star / data$C) + best_fits$R_d
  data$A_regen <- calculate_j(
    PPFD = mean(data$PPFD), alpha = alpha,
    J_max = best_fits$J_max, theta_J = theta_J
  ) *
    (data$C - gamma_star) / (4 * data$C + 8 * gamma_star) + best_fits$R_d
  data$A_tpu <- 3 * best_fits$V_TPU /
    (1 - 0.5 * (1 + 3 * alpha_g) * (2 * gamma_star / data$C)) *
    (1 - gamma_star / data$C) + best_fits$R_d
  # Calculate gross photosynthetic rates
  data$W_carbox <- data$A_carbox - best_fits$R_d
  data$W_regen <- data$A_regen - best_fits$R_d
  data$W_tpu <- data$A_tpu - best_fits$R_d
  # Create empty variable for modelled CO2 assimilation
  data$A_model <- rep(NA, nrow(data))
  # To avoid issues with graphing and cases where W_j drops below W_c
  # at very low CO2, for the first few points, A_model is calculated
  # with W_c only - plantecophys took an approach where Aj was fixed
  # on the graph until a certain C_i to avoid this same issue.
  for (i in 1:(best_fits$V_cmax_pts - 2)) {
    data$A_model[i] <- data$W_carbox[i] + best_fits$R_d
  }
  if (best_fits$V_TPU == 1000) {
    for (i in (best_fits$V_cmax_pts - 1):nrow(data)) {
      data$A_model[i] <- min(data$W_carbox[i], data$W_regen[i]) + best_fits$R_d
    }
  } else {
    for (i in (best_fits$V_cmax_pts - 1):nrow(data)) {
      data$A_model[i] <- min(
        data$W_carbox[i], data$W_regen[i],
        data$W_tpu[i]
      ) + best_fits$R_d
    }
  }
  # Assign best fit to output element 1
  outputs[[1]] <- best_fits
  # Assign graph to output element 2
  outputs[[2]] <- ggplot(data, aes(x = C_i, y = A_model)) +
    scale_y_continuous(limits = c(
      min(c(data$A_model, data$A_net)) - 3,
      max(c(data$A_model, data$A_net)) + 3
    )) +
    geom_line(aes(color = "black"), linewidth = 4) +
    geom_line(aes(y = A_carbox, color = "blue"), linewidth = 2) +
    geom_line(aes(y = A_regen, color = "orange"), linewidth = 2) +
    geom_line(aes(y = A_tpu, color = "red"), linewidth = 2) +
    geom_point(aes(y = A_net),
      color = "black", fill = "white",
      size = 2, shape = 21
    ) +
    scale_color_manual(
      labels = c("Amod", "Ac", "Aj", "Ap", "Anet"),
      values = c(
        "black", "blue", "orange",
        "red", "white"
      )
    ) +
    theme_bw() +
    theme(legend.title = element_blank())
  # Assign dataframe to output element 3
  outputs[[3]] <- data
  # Add names to list output
  names(outputs) <- c("Fitted Parameters", "Plot", "Data")
  # Return output list
  return(outputs)
} # End function
