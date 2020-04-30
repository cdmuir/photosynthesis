#' Fitting the Ball-Berry model of stomatal conductance
#'
#' @param data Dataframe
#' @param varnames Variable names. varnames = list(A_net = "A_net", Ca = "Ca",
#' gsw = "gsw", RH = "RH") where A_net is net CO2 assimilation, Ca is CO2
#' concentration at the leaf surface in umol mol-1, gsw is stomatal conductance
#' to H2O, and RH is relative humidity as a proportion.
#'
#' @return fit_gs_mod_ballberry fits the Ball et al. (1987) model of stomatal
#' conductance. Note that RH should be converted to a proportion before fitting
#' the model.
#' 
#' Ball JT, Woodrow IE, Berry JA. 1987. A model predicting stomatal 
#' conductance and its contribution to the control of photosynthesis
#' under different environmental conditions, in Progress in 
#' Photosynthesis Research, Proceedings of the VII International 
#' Congress on Photosynthesis, vol. 4, edited by I. Biggins, pp. 
#' 221–224, Martinus Nijhoff, Dordrecht, Netherlands.
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
#' #Convert RH to proportion
#' data$RH <- data$RHcham / 100
#' 
#' #Fit the Ball-Berry model
#' fit <- fit_gs_mod_ballberry(data = data,
#'                             varnames = list(A_net = "A",
#'                                             Ca = "Ca",
#'                                             gsw = "gsw",
#'                                             RH = "RH"))
#' 
#' #View model summary
#' summary(fit[[1]])
#' 
#' #View model output
#' fit[[2]]
#' 
#' #View graph
#' fit[[3]]
#' 
#' #Fit many gsw models
#' #Set your grouping variable
#' #Here we are grouping by Qin and individual
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#' 
#' fits <- fit_many(data,
#'                  varnames = list(A_net = "A",
#'                                  Ca = "Ca",
#'                                  gsw = "gsw",
#'                                  RH = "RH"),
#'                  funct = fit_gs_mod_ballberry,
#'                  group = "Q_2")
#' 
#' #Look at 2nd group information
#' #Model summary
#' summary(fits[[2]][[1]])
#' 
#' #Model parameters
#' fits[[2]][[2]]
#' 
#' #Graph
#' fits[[2]][[3]]
#' 
#' #Compile parameter outputs
#' pars <- compile_data(data = fits,
#'                      output_type = "dataframe",
#'                      list_element = 2)
#' 
#' #Convert group variable back to numeric
#' pars$ID <- as.numeric(pars$ID)
#' 
#' #Take quick look at light response of intercept parameters
#' plot(g0 ~ ID, pars)
#' 
#' #Compile graphs
#' graphs <- compile_data(data = fits,
#'                        output_type = "list",
#'                        list_element = 3)
#' 
#' #Look at 3rd graph
#' graphs[[3]]
#' 
#' }
fit_gs_mod_ballberry <- function(data,
                                 varnames = list(A_net = "A_net",
                                                 Ca = "Ca",
                                                 gsw = "gsw",
                                                 RH = "RH")){
  #Locally bind variables - avoids notes on check package
  A_net <- NULL
  Ca <- NULL
  gsw <- NULL
  RH <- NULL
  
  #Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$Ca <- data[, varnames$Ca]
  data$gsw <- data[, varnames$gsw]
  data$RH <- data[, varnames$RH]
  
  #Create empty list
  fit <- list(NULL)
  
  #Assign linera regression model to element 1
  fit[[1]] <- lm(data = data,
                 gsw ~ gs_mod_ballberry(A_net = A_net,
                                        Ca = Ca,
                                        RH = RH))
  
  #Extract coefficients
  g0 <- coef(fit[[1]])[[1]]
  g1 <- coef(fit[[1]])[[2]]
  
  #Create list element of coefficients
  fit[[2]] <- as.data.frame(cbind(g0, g1))
  
  #Assign graph to element 3
  fit[[3]] <- ggplot(data, aes(x = I(A_net * Ca * RH), y = gsw)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_point() +
    theme_bw()
  
  #Assign names to list
  names(fit) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(fit)
}
