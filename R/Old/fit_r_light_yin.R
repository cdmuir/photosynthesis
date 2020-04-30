#' Estimating light respiration through the Yin method
#'
#' @param data Dataframe
#' @param varnames List of variable names to use
#' @param Q_inc_lower Lower light intensity limit for curve fit
#' @param Q_inc_upper Upper light intensity limit for curve fit
#'
#' @return fit_r_light_yin estimates light respiration according
#'     to the Yin et al. (2009, 2011) modifications of the Kok
#'     method. The modification uses fluorescence data to get a 
#'     better estimate of light respiration. Note that respiration
#'     output should be negative here to denote an efflux of CO2.
#'     
#'     Yin X, Struik PC, Romero P, Harbinson J, Evers JB, van der Putten 
#'     PEL, Vos J. 2009. Using combined measurements of gas exchange and
#'     chlorophyll fluorescence to estimate parameters of a biochemical C3
#'     photosynthesis model: a critical appraisal and a new integrated 
#'     approach applied to leaves in a wheat (Triticum aestivum) canopy. 
#'     Plant Cell Environ 32:448-464
#'     
#'     Yin X, Sun Z, Struik PC, Gu J. 2011. Evaluating a new method to 
#'     estimate the rate of leaf respiration in the light by analysis of
#'     combined gas exchange and chlorophyll fluorescence measurements. 
#'     Journal of Experimental Botany 62: 3489–3499
#' @export
#' 
#' @examples \dontrun{
#' #Read in your data
#' #Note that this data is coming from data supplied by the package
#' #hence the complicated argument in read.csv()
#' #This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
#'                              package = "plantecophystools"))
#' 
#' #Fit light respiration with Yin method
#' r_light <- fit_r_light_yin(data = data,
#'                            varnames = list(A_net = "A",
#'                                            Q_inc = "Qin",
#'                                            phi_PSII = "PhiPS2"),
#'                            Q_inc_lower = 20,
#'                            Q_inc_upper = 250)
#' }
#'
fit_r_light_yin <- function(data,
                            varnames = list(A_net = "A_net",
                                            Q_inc = "Q_inc",
                                            phi_PSII = "phi_PSII"),
                            Q_inc_lower = 40,
                            Q_inc_upper = 100){
  #Set variable names
  data$A_net <- data[, varnames$A_net]
  data$phi_PSII <- data[, varnames$phi_PSII]
  data$Q_inc <- data[, varnames$Q_inc]
  #Reduce data to within Q_inc range
  data_use <- data[data$Q_inc < Q_inc_upper, ]
  data_use <- data_use[data_use$Q_inc > Q_inc_lower, ]
  #Calculate x-variable for Yin method
  data_use$x_var <- data_use$Q_inc * data_use$phi_PSII / 4
  #Fit linear model for Yin method
  model <- lm(A_net ~ x_var, data = data_use)
  r_light <- coef(model)[1]
  return(r_light)
}
