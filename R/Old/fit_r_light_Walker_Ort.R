#' Estimating light respiration and GammaStar
#'
#' @param data Dataframe
#' @param varnames List of variable names
#' @param Patm Atmospheric pressure in kPa
#' @param Ci_threshold Threshold Ci (in umol mol-1) to cut data to
#' linear region for fitting light respiration and GammaStar
#'
#' @return fit_r_light_WalkerOrt estimates light respiration and
#'     GammaStar according to Walk & Ort (2015) using a slope-
#'     intercept regression method to find the intercept of multiple
#'     ACi curves run at multiple light intensities. Output GammaStar and
#'     respiration should be negative If output respiration is positive
#'     this could indicate issues (i.e. leaks) in the gas exchange
#'     measurements. GammaStar is output in umol mol-1, and respiration
#'     is output in umol m-2 s-1 of respiratory flux. Output is a list
#'     containing the slope intercept regression model, a graph of the fit,
#'     and estimates of the coefficients.
#'     
#'     NOTE: if using Ci, the output value is technically Cistar. You
#'     need to use Cc to get GammaStar. Also note, however, that the
#'     convention in the field is to completely ignore this note.
#'     
#'     Walker BJ, Ort DR. 2015. Improved method for measuring the apparent
#'     CO2 photocompensation point resolves the impact of multiple internal
#'     conductances to CO2 to net gas exchange. Plant Cell Environ 38:2462-
#'     2474
#'     
#' @importFrom nlme lmList
#' @importFrom stats coef
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
#' #Fit the Walker-Ort method for GammaStar and light respiration
#' walker_ort <- fit_r_light_WalkerOrt(data,
#'                                     varnames = list(A_net = "A",
#'                                                     Ci = "Ci",
#'                                                     Q_inc = "Qin"))
#' #Extract model
#' summary(walker_ort[[1]])
#' 
#' #View graph
#' walker_ort[[2]]
#' 
#' #View coefficients
#' walker_ort[[3]]
#' }
#' 
fit_r_light_WalkerOrt <- function(data,
                              varnames = list(A_net = "A_net",
                                              Ci = "Ci",
                                              Q_inc = "Q_inc"),
                              Patm = 100,
                              Ci_threshold = 300){
  #Set variable names
  data$A_net <- data[, varnames$A_net]
  data$Ci <- data[, varnames$Ci]
  data$Q_inc <- data[, varnames$Q_inc]
  
  #Locally define slope and intercept for slope-intercept regression
  #This gets rid of a note in the R CMD CHECK
  Slope <- NULL
  Intercept <-NULL
  
  #Restrict data analysis by a threshold Ci
  data_use <- data[data$Ci < Ci_threshold,]
  #Convert Ci to units of Pa
  data_use$Ci <- data_use$Ci / 1000000 * Patm * 1000
  #Set Q_inc as factor for grouping & round
  #Q_inc to nearest 10s
  data_use$Q_inc <- round(data_use$Q_inc, digits = -1)
  data_use$Q_inc <- as.factor(data_use$Q_inc)
  #Construct regressions on the pseudolinear portions of
  #the ACi curves
  model <- lmList(A_net ~ Ci | Q_inc, data = data_use)
  #Extract coefficients
  coefs <- coef(model)
  colnames(coefs) <- c("Intercept", "Slope")
  coefs$Q_inc <- rownames(coefs)
  #Create output list
  output <- list(NULL)
  #Run slope-intercept regression model, assign to element 1
  output[[1]] <- lm(Intercept ~ Slope,
                              data = coefs)
  
  #Create graph, assign to element 2
  output[[2]] <- ggplot(coefs, aes(x = Slope, y = Intercept)) +
    labs(x = "Slope", y = "Intercept") +
    geom_smooth(method = "lm", size = 2) +
    geom_point(size = 3) +
    theme_bw()
  
  #Extract coefficients as per Walker and Ort 2015
  #But convert Cistar to umol mol-1
  GammaStar <- -coef(output[[1]])[2] / (Patm * 1000) * 1000000
  r_light <- coef(output[[1]])[1]
  output[[3]] <- as.data.frame(cbind(GammaStar, r_light))
  #Return output
  return(output)
}
