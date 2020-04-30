#' Estimating light respiration with the Kok method
#'
#' @param data Dataframe
#' @param varnames List of variable names
#' @param Q_inc_lower Lower light intensity limit for estimating Rlight
#' @param Q_inc_upper Upper light intensity limit for estimating Rlight
#'
#' @return fit_r_light_kok estimates light respiration using the Kok method
#'     (Kok, 1956). The Kok method involves looking for a breakpoint in the
#'     light response of net CO2 assimilation at very low light intensities
#'     and extrapolating from data above the breakpoint to estimate light
#'     respiration as the y-intercept. r_light value should be negative,
#'     denoting an efflux of CO2.
#'     Kok B. 1956. On the inhibition of photosynthesis by intense light.
#'     Biochimica et Biophysica Acta 21: 234–244
#' @importFrom stats coef
#' @importFrom stats lm
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
#' #Fit light respiration with Kok method
#' r_light <- fit_r_light_kok(data = data,
#'                            varnames = list(A_net = "A",
#'                                            Q_inc = "Qin"),
#'                            Q_inc_lower = 20,
#'                            Q_inc_upper = 150)
#' #Return r_light
#' r_light
#' }
#'
fit_r_light_kok <- function(data,
                            varnames = list(A_net = "A_net",
                                            Q_inc = "Q_inc"),
                            Q_inc_lower = 40,
                            Q_inc_upper = 100){
  #Set variable names
  data$A_net <- data[, varnames$A_net]
  data$Q_inc <- data[, varnames$Q_inc]
  #Reduce data to within Q_inc range
  data_use <- data[data$Q_inc < Q_inc_upper, ]
  data_use <- data_use[data_use$Q_inc > Q_inc_lower, ]
  #Linear regression to estimate r_light (intercept)
  model <- lm(A_net ~ Q_inc, data = data_use)
  r_light <- coef(model)[1]
  #Output light respiration value
  return(r_light)
}
