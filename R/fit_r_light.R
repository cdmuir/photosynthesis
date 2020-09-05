#' Estimating light respiration
#'
#' @param data Dataframe
#' @param varnames List of variable names
#' @param PPFD_lower Lower light intensity limit for estimating Rlight
#' (Kok & Yin)
#' @param PPFD_upper Upper light intensity limit for estimating Rlight
#' (Kok & Yin)
#'
#' @param P Atmospheric pressure in kPa (Walker & Ort, 2015)
#' @param C_i_threshold Threshold C_i (in umol mol-1) to cut data to
#' linear region for fitting light respiration and gamma_star
#' (Walker & Ort, 2015)
#'
#' @return fit_r_light_kok estimates light respiration using the Kok method
#' (Kok, 1956). The Kok method involves looking for a breakpoint in the
#' light response of net CO2 assimilation at very low light intensities
#' and extrapolating from data above the breakpoint to estimate light
#' respiration as the y-intercept. r_light value should be negative,
#' denoting an efflux of CO2.
#'
#' fit_r_light_WalkerOrt estimates light respiration and
#' GammaStar according to Walk & Ort (2015) using a slope-
#' intercept regression method to find the intercept of multiple
#' ACi curves run at multiple light intensities. Output GammaStar and
#' respiration should be negative If output respiration is positive
#' this could indicate issues (i.e. leaks) in the gas exchange
#' measurements. GammaStar is output in umol mol-1, and respiration
#' is output in umol m-2 s-1 of respiratory flux. Output is a list
#' containing the slope intercept regression model, a graph of the fit,
#' and estimates of the coefficients. NOTE: if using C_i, the output value
#' is technically C_istar. You need to use Cc to get GammaStar. Also note,
#' however, that the convention in the field is to completely ignore this note.
#'
#' fit_r_light_yin estimates light respiration according
#' to the Yin et al. (2009, 2011) modifications of the Kok
#' method. The modification uses fluorescence data to get a
#' better estimate of light respiration. Note that respiration
#' output should be negative here to denote an efflux of CO2.
#'
#' @references
#' Kok B. 1956. On the inhibition of photosynthesis by intense light.
#' Biochimica et Biophysica Acta 21: 234–244
#'
#' Walker BJ, Ort DR. 2015. Improved method for measuring the apparent
#' CO2 photocompensation point resolves the impact of multiple internal
#' conductances to CO2 to net gas exchange. Plant Cell Environ 38:2462-
#' 2474
#'
#' Yin X, Struik PC, Romero P, Harbinson J, Evers JB, van der Putten
#' PEL, Vos J. 2009. Using combined measurements of gas exchange and
#' chlorophyll fluorescence to estimate parameters of a biochemical C3
#' photosynthesis model: a critical appraisal and a new integrated
#' approach applied to leaves in a wheat (Triticum aestivum) canopy.
#' Plant Cell Environ 32:448-464
#'
#' Yin X, Sun Z, Struik PC, Gu J. 2011. Evaluating a new method to
#' estimate the rate of leaf respiration in the light by analysis of
#' combined gas exchange and chlorophyll fluorescence measurements.
#' Journal of Experimental Botany 62: 3489–3499
#'
#' @importFrom nlme lmList
#' @importFrom stats coef
#' @importFrom stats lm
#'
#' @examples
#' \donttest{
#' # FITTING KOK METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit light respiration with Kok method
#' r_light <- fit_r_light_kok(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin"
#'   ),
#'   PPFD_lower = 20,
#'   PPFD_upper = 150
#' )
#' # Return r_light
#' r_light
#'
#' # FITTING WALKER-ORT METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit the Walker-Ort method for GammaStar and light respiration
#' walker_ort <- fit_r_light_WalkerOrt(data,
#'   varnames = list(
#'     A_net = "A",
#'     C_i = "Ci",
#'     PPFD = "Qin"
#'   )
#' )
#' # Extract model
#' summary(walker_ort[[1]])
#'
#' # View graph
#' walker_ort[[2]]
#'
#' # View coefficients
#' walker_ort[[3]]
#'
#' # FITTING THE YIN METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit light respiration with Yin method
#' r_light <- fit_r_light_yin(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin",
#'     phi_PSII = "PhiPS2"
#'   ),
#'   PPFD_lower = 20,
#'   PPFD_upper = 250
#' )
#' }
#'
#' @rdname fit_r_light
#' @export
fit_r_light_kok <- function(data,
                            varnames = list(
                              A_net = "A_net",
                              PPFD = "PPFD"
                            ),
                            PPFD_lower = 40,
                            PPFD_upper = 100) {
  # Set variable names
  data$A_net <- data[, varnames$A_net]
  data$PPFD <- data[, varnames$PPFD]
  # Reduce data to within PPFD range
  data_use <- data[data$PPFD < PPFD_upper, ]
  data_use <- data_use[data_use$PPFD > PPFD_lower, ]
  # Linear regression to estimate r_light (intercept)
  model <- lm(A_net ~ PPFD, data = data_use)
  r_light <- coef(model)[1]
  # Output light respiration value
  return(r_light)
}

#' @rdname fit_r_light
#' @export
fit_r_light_WalkerOrt <- function(data,
                                  varnames = list(
                                    A_net = "A_net",
                                    C_i = "C_i",
                                    PPFD = "PPFD"
                                  ),
                                  P = 100,
                                  C_i_threshold = 300) {
  # Set variable names
  data$A_net <- data[, varnames$A_net]
  data$C_i <- data[, varnames$C_i]
  data$PPFD <- data[, varnames$PPFD]

  # Locally define slope and intercept for slope-intercept regression
  # This gets rid of a note in the R CMD CHECK
  Slope <- NULL
  Intercept <- NULL

  # Restrict data analysis by a threshold C_i
  data_use <- data[data$C_i < C_i_threshold, ]
  # Convert C_i to units of Pa
  data_use$C_i <- data_use$C_i / 1000000 * P * 1000
  # Set PPFD as factor for grouping & round
  # PPFD to nearest 10s
  data_use$PPFD <- round(data_use$PPFD, digits = -1)
  data_use$PPFD <- as.factor(data_use$PPFD)
  # Construct regressions on the pseudolinear portions of
  # the ACi curves
  model <- lmList(A_net ~ C_i | PPFD, data = data_use)
  # Extract coefficients
  coefs <- coef(model)
  colnames(coefs) <- c("Intercept", "Slope")
  coefs$PPFD <- rownames(coefs)
  # Create output list
  output <- list(NULL)
  # Run slope-intercept regression model, assign to element 1
  output[[1]] <- lm(Intercept ~ Slope,
    data = coefs
  )
  # Create graph, assign to element 2
  output[[2]] <- ggplot(coefs, aes(x = Slope, y = Intercept)) +
    labs(x = "Slope", y = "Intercept") +
    geom_smooth(method = "lm", size = 2) +
    geom_point(size = 3) +
    theme_bw()
  # Extract coefficients as per Walker and Ort 2015
  # But convert C_istar to umol mol-1
  GammaStar <- -coef(output[[1]])[2] / (P * 1000) * 1000000
  r_light <- coef(output[[1]])[1]
  output[[3]] <- as.data.frame(cbind(GammaStar, r_light))
  # Return output
  return(output)
}

#' @rdname fit_r_light
#' @export
fit_r_light_yin <- function(data,
                            varnames = list(
                              A_net = "A_net",
                              PPFD = "PPFD",
                              phi_PSII = "phi_PSII"
                            ),
                            PPFD_lower = 40,
                            PPFD_upper = 100) {
  # Set variable names
  data$A_net <- data[, varnames$A_net]
  data$phi_PSII <- data[, varnames$phi_PSII]
  data$PPFD <- data[, varnames$PPFD]
  # Reduce data to within PPFD range
  data_use <- data[data$PPFD < PPFD_upper, ]
  data_use <- data_use[data_use$PPFD > PPFD_lower, ]
  # Calculate x-variable for Yin method
  data_use$x_var <- data_use$PPFD * data_use$phi_PSII / 4
  # Fit linear model for Yin method
  model <- lm(A_net ~ x_var, data = data_use)
  r_light <- coef(model)[1]
  return(r_light)
}
