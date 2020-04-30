#' Fitting stomatal conductance models
#'
#' @param data Dataframe
#' @param varnames Variable names. 
#' 
#' For the Ball-Berry model: varnames = list(A_net = "A_net", C_air = "C_air",
#' g_sw = "g_sw", RH = "RH") where A_net is net CO2 assimilation, C_air is CO2
#' concentration at the leaf surface in umol mol-1, g_sw is stomatal conductance
#' to H2O, and RH is relative humidity as a proportion.
#' 
#' For the Leuning model: varnames = list(A_net = "A_net", C_air = "C_air",
#' g_sw = "g_sw", VPD = "VPD") where A_net is net CO2 assimilation, C_air is CO2
#' concentration at the leaf surface in umol mol-1, g_sw is stomatal conductance
#' to H2O, and VPD is leaf to air vapor pressure deficit in kPa.
#' 
#' For the Medlyn et al. 2011 models: varnames = list(A_net = "A_net", C_air = "C_air",
#' g_sw = "g_sw", VPD = "VPD") where A_net is net CO2 assimilation, C_air is CO2
#' concentration at the leaf surface in umol mol-1, g_sw is stomatal conductance
#' to H2O, and VPD is leaf to air vapor pressure deficit in kPa.
#' @param D0 Vapor pressure sensitivity of stomata (Leuning 1995)
#' @param full_model Fit the full model? (Medlyn et al. 2011)
#'
#' @return fit_gs_mod_ballberry fits the Ball et al. (1987) model of stomatal
#' conductance. Note that RH should be converted to a proportion before fitting
#' the model.
#' 
#' fit_gs_mod_leuning fits the Leuning (1995) model of stomatal
#' conductance.
#' 
#' fit_gs_mod_medlyn fits the Medlyn et al (2011) model of stomatal
#' conductance. Both the reduced and full models can be fit.
#' 
#' REFERENCES
#' 
#' Ball JT, Woodrow IE, Berry JA. 1987. A model predicting stomatal 
#' conductance and its contribution to the control of photosynthesis
#' under different environmental conditions, in Progress in 
#' Photosynthesis Research, Proceedings of the VII International 
#' Congress on Photosynthesis, vol. 4, edited by I. Biggins, pp. 
#' 221–224, Martinus Nijhoff, Dordrecht, Netherlands.
#' 
#' Leuning R. 1995. A critical appraisal of a coupled stomatal-
#' photosynthesis model for C3 plants. Plant Cell Environ 18:339-357
#' 
#' Medlyn BE, Duursma RA, Eamus D, Ellsworth DS, Prentice IC, Barton
#' CVM, Crous KY, Angelis PD, Freeman M, Wingate L. 2011. Reconciling
#' the optimal and empirical approaches to modelling stomatal 
#' conductance. Glob Chang Biol 17:2134-2144
#' 
#' @importFrom minpack.lm nlsLM
#' 
#' @examples \dontrun{
#' #Read in your data
#' #Note that this data is coming from data supplied by the package
#' #hence the complicated argument in read.csv()
#' #This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
#'                              package = "plantecophystools"))
#' 
#' #FITTING THE BALL-BERRY MODEL
#' 
#' #Convert RH to proportion
#' data$RH <- data$RHcham / 100
#' 
#' #Fit the Ball-Berry model
#' fit <- fit_gs_mod_ballberry(data = data,
#'                             varnames = list(A_net = "A",
#'                                             C_air = "Ca",
#'                                             g_sw = "gsw",
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
#' #Fit many g_sw models
#' #Set your grouping variable
#' #Here we are grouping by Qin and individual
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#' 
#' fits <- fit_many(data,
#'                  varnames = list(A_net = "A",
#'                                  C_air = "Ca",
#'                                  g_sw = "gsw",
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
#' 
#' #FITTING THE LEUNING MODEL
#' 
#' fit <- fit_gs_mod_leuning(data = data,
#'                             D0 = 3,
#'                             varnames = list(A_net = "A",
#'                                             C_air = "Ca",
#'                                             g_sw = "gsw",
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
#' #Fit many g_sw models
#' #Set your grouping variable
#' #Here we are grouping by Qin and individual
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#' 
#' fits <- fit_many(data,
#'                  varnames = list(A_net = "A",
#'                                  C_air = "Ca",
#'                                  g_sw = "gsw",
#'                                  VPD = "VPDleaf"),
#'                  funct = fit_gs_mod_leuning,
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
#' #FITTING THE FULL MEDLYN MODEL
#' fit <- fit_gs_mod_medlyn(data = data,
#'                             full_model = TRUE,
#'                             varnames = list(A_net = "A",
#'                                             C_air = "Ca",
#'                                             g_sw = "gsw",
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
#' #Fit many g_sw models
#' #Set your grouping variable
#' #Here we are grouping by Qin and individual
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#' 
#' fits <- fit_many(data,
#'                  varnames = list(A_net = "A",
#'                                  C_air = "Ca",
#'                                  g_sw = "gsw",
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
#' 
#' @rdname fit_gs_models
#' @export
fit_gs_mod_ballberry <- function(data,
                                 varnames = list(A_net = "A_net",
                                                 C_air = "C_air",
                                                 g_sw = "g_sw",
                                                 RH = "RH")){
  #Locally bind variables - avoids notes on check package
  A_net <- NULL
  C_air <- NULL
  g_sw <- NULL
  RH <- NULL
  
  #Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$C_air <- data[, varnames$C_air]
  data$g_sw <- data[, varnames$g_sw]
  data$RH <- data[, varnames$RH]
  
  #Create empty list
  fit <- list(NULL)
  
  #Assign linera regression model to element 1
  fit[[1]] <- lm(data = data,
                 g_sw ~ gs_mod_ballberry(A_net = A_net,
                                        C_air = C_air,
                                        RH = RH))
  
  #Extract coefficients
  g0 <- coef(fit[[1]])[[1]]
  g1 <- coef(fit[[1]])[[2]]
  
  #Create list element of coefficients
  fit[[2]] <- as.data.frame(cbind(g0, g1))
  
  #Assign graph to element 3
  fit[[3]] <- ggplot(data, aes(x = I(A_net * C_air * RH), y = g_sw)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_point() +
    theme_bw()
  
  #Assign names to list
  names(fit) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(fit)
}

#' @rdname fit_gs_models
#' @export
fit_gs_mod_leuning <- function(data,
                               D0,
                               varnames = list(A_net = "A_net",
                                               C_air = "C_air",
                                               g_sw = "g_sw",
                                               VPD = "VPD")){
  #Locally bind variables - avoids notes on check package
  A_net <- NULL
  C_air <- NULL
  g_sw <- NULL
  VPD <- NULL
  
  #Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$C_air <- data[, varnames$C_air]
  data$g_sw <- data[, varnames$g_sw]
  data$VPD <- data[, varnames$VPD]
  
  #Create empty list
  fit <- list(NULL)
  
  #Assign regression model to element 1
  fit[[1]] <- lm(data = data,
                 g_sw ~ gs_mod_leuning(A_net = A_net,
                                      C_air = C_air,
                                      D0 = D0,
                                      VPD = VPD))
  
  #Extract coefficients
  g0 <- coef(fit[[1]])[[1]]
  g1 <- coef(fit[[1]])[[2]]
  
  #Assign coefficients to element 2
  fit[[2]] <- as.data.frame(cbind(g0, g1))
  
  #Assign graph to element 3
  fit[[3]] <- ggplot(data, aes(x = I(A_net / (C_air * (1 + VPD * D0))), y = g_sw)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_point() +
    theme_bw()
  
  #Assign names to list elements
  names(fit) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(fit)
}

#' @rdname fit_gs_models
#' @export
fit_gs_mod_medlyn <- function(data,
                              varnames = list(A_net = "A_net",
                                              C_air = "C_air",
                                              g_sw = "g_sw",
                                              VPD = "VPD"),
                              full_model = FALSE){
  #Locally bind variables - avoids notes on check package
  A_net <- NULL
  C_air <- NULL
  g_sw <- NULL
  VPD <- NULL
  
  #Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$C_air <- data[, varnames$C_air]
  data$g_sw <- data[, varnames$g_sw]
  data$VPD <- data[, varnames$VPD]
  
  #Create empty list
  fit <- list(NULL)
  
  if(full_model == FALSE){
    #Fit model, assign to element 1
    fit[[1]] <- nlsLM(data = data,
                      g_sw ~ gs_mod_opti(A_net = A_net,
                                        C_air = C_air,
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
    fit[[3]] <- ggplot(data, aes(x = I(1.6 / (sqrt(VPD)) * (A_net / C_air) + 1.6 * (A_net / C_air)), y = g_sw)) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
    
  } else {
    #Fit model, assign to element 1
    fit[[1]] <- nlsLM(data = data,
                      g_sw ~ gs_mod_optifull(A_net = A_net,
                                            C_air = C_air,
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
    fit[[3]] <- ggplot(data, aes(x = I(1.6 / ((VPD) ^ (1 - gk)) * (A_net / C_air) + 1.6 * (A_net / C_air)), y = g_sw)) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
  }
  
  #Assign names to list elements
  names(fit) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(fit)
}
