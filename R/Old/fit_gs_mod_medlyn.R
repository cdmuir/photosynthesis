#' Fitting the Medlyn model of stomatal conductance
#'
#' @param data Dataframe
#' @param varnames Variable names. varnames = list(A_net = "A_net", Ca = "Ca",
#' gsw = "gsw", VPD = "VPD") where A_net is net CO2 assimilation, Ca is CO2
#' concentration at the leaf surface in umol mol-1, gsw is stomatal conductance
#' to H2O, and VPD is leaf to air vapor pressure deficit in kPa.
#' @param full_model Fit the full model?
#'
#' @return fit_gs_mod_medlyn fits the Medlyn et al (2011) model of stomatal
#' conductance. Both the reduced and full models can be fit.
#' 
#' Medlyn BE, Duursma RA, Eamus D, Ellsworth DS, Prentice IC, Barton
#' CVM, Crous KY, Angelis PD, Freeman M, Wingate L. 2011. Reconciling
#' the optimal and empirical approaches to modelling stomatal 
#' conductance. Glob Chang Biol 17:2134-2144
#' 
#' @importFrom minpack.lm nlsLM
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
#' 
#' #Fit the full Medlyn model
#' fit <- fit_gs_mod_medlyn(data = data,
#'                             full_model = TRUE,
#'                             varnames = list(A_net = "A",
#'                                             Ca = "Ca",
#'                                             gsw = "gsw",
#'                                             VPD = "VPDleaf"))
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
#'                                  VPD = "VPDleaf"),
#'                  funct = fit_gs_mod_medlyn,
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
fit_gs_mod_medlyn <- function(data,
                              varnames = list(A_net = "A_net",
                                              Ca = "Ca",
                                              gsw = "gsw",
                                              VPD = "VPD"),
                              full_model = FALSE){
  #Locally bind variables - avoids notes on check package
  A_net <- NULL
  Ca <- NULL
  gsw <- NULL
  VPD <- NULL
  
  #Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$Ca <- data[, varnames$Ca]
  data$gsw <- data[, varnames$gsw]
  data$VPD <- data[, varnames$VPD]
  
  #Create empty list
  fit <- list(NULL)
  
  if(full_model == FALSE){
  #Fit model, assign to element 1
  fit[[1]] <- nlsLM(data = data,
                    gsw ~ gs_mod_opti(A_net = A_net,
                                      Ca = Ca,
                                      VPD = VPD,
                                      g0,
                                      g1),
                    start = list(g0 = 0,
                                 g1 = 1),
                    control = nls.control(maxiter = 1000))
  
  #Extract coefficients and make dataframe
  g0 <- coef(fit[[1]])[[1]]
  g1 <- coef(fit[[1]])[[2]]
  
  #Assign coefficients to element 2
  fit[[2]] <- as.data.frame(cbind(g0, g1))
  
  #Create graph, assign to element 3
  fit[[3]] <- ggplot(data, aes(x = I(1.6 / (sqrt(VPD)) * (A_net / Ca) + 1.6 * (A_net / Ca)), y = gsw)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_point() +
    theme_bw()
  
  } else {
    #Fit model, assign to element 1
    fit[[1]] <- nlsLM(data = data,
                      gsw ~ gs_mod_optifull(A_net = A_net,
                                        Ca = Ca,
                                        VPD = VPD,
                                        g0,
                                        g1,
                                        gk),
                      start = list(g0 = 0,
                                   g1 = 1,
                                   gk = 1),
                      control = nls.control(maxiter = 1000))
    
    #Extract coefficients and make dataframe
    g0 <- coef(fit[[1]])[[1]]
    g1 <- coef(fit[[1]])[[2]]
    gk <- coef(fit[[1]])[[3]]
    
    #Assign coefficients to element 2
    fit[[2]] <- as.data.frame(cbind(g0, g1, gk))
    
    #Create graph, assign to element 3
    fit[[3]] <- ggplot(data, aes(x = I(1.6 / ((VPD) ^ (1 - gk)) * (A_net / Ca) + 1.6 * (A_net / Ca)), y = gsw)) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
  }
  
  #Assign names to list elements
  names(fit) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(fit)
}
