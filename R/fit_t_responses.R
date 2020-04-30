#' Fitting temperature response functions
#'
#' @param data Dataframe
#' @param varnames Variable names for the parameter to fit and leaf temperature
#' @param start List of starting values for NLS curve fitting
#' @param title Title for the graph
#' 
#' @param start Starting values for curve fitting (for Medlyn Arrhenius)
#' @param setvar Set a variable to constant? One of "none", "Hd" or "dS" (for Medlyn Arrhenius)
#' @param Hd_set Value to set for Hd in J/mol (for Medlyn Arrhenius)
#' @param dS_set Value to set for dS in J/mol (for Medlyn Arrhenius)
#'
#' @return fit_t_response_arrhenius fits an Arrhenius temperature response
#' to data
#' 
#' fit_t_response_heskel fits a polynomial to log-transformed temperature responses
#' based on Heskel et al. 2016
#' 
#' fit_t_response_kruse fits the Kruse et al. 2008 model of
#' temperature responses
#' 
#' fit_t_response_medlyn fits the Medlyn et al. 2002 temperature
#' response model to data.
#' 
#' fit_t_response_mmrt fits the Macromolecular Rate Theory equation as redefined in
#' Liang et al. 2018, and described in Hobbs et al. 2013.
#' 
#' fit_t_response_quad is used to fit a 2nd order polynomial to
#' temperature response data
#' 
#' fit_t_response_topt fits the peaked Arrhenius model from
#' Medlyn et al. 2002 with kopt and Topt.
#' 
#' REFERENCES
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#' 
#' Heskel MA, O'Sullivan OS, Reich PB, Tjoelker MG, Weerasinghe LK,
#' Penillard A, Egerton JJG, Creek D, Bloomfield KJ, Xiang J, Sinca F,
#' Stangl ZR, la Torre AM, Griffin KL, Huntingford C, Hurry V, Meir P,
#' Turnbull MH, Atkin OK. 2016. Convergence in the temperature response
#' of leaf respiration across biomes and plant functional types. PNAS
#' 113:3832-3837
#' 
#' Hobbs JK, Jiao W, Easter AD, Parker EJ, Schipper LA, Arcus VL.
#' 2013. Change in heat capacity for enzyme catalysis determines
#' temperature dependence of enzyme catalyzed rates. ACS Chemical
#' Biology 8:2388-2393.
#' 
#' Kruse J, Adams MA. 2008. Three parameters comprehensively describe
#' the temperature response of respiratory oxygen reduction. Plant 
#' Cell Environ 31:954-967
#' 
#' Liang LL, Arcus VL, Heskel MA, O'Sullivan OS, Weerasinghe LK,
#' Creek D, Egerton JJG, Tjoelker MG, Atkin OK, Schipper LA. 2018.
#' Macromolecular rate theory (MMRT) provides a thermodynamics
#' rationale to underpin the convergent temperature response in
#' plant leaf respiration. Glob Chang Biol 24:1538-1547
#' 
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' 
#' 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom minpack.lm nlsLM
#' @importFrom stats nls.control
#' 
#' @examples \dontrun{
#' #FITTING ARRHENIUS RESPONSE CURVE
#' out <- fit_t_response_arrhenius(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING HESKEL RESPONSE
#' out <- fit_t_response_heskel(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING KRUSE RESPONSE
#' out <- fit_t_response_kruse(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING MEDLYN ARRHENIUS
#' out <- fit_t_response_medlyn(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"),
#'                 setvar = "dS",
#'                 dS_set = 660)
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING MMRT
#' out <- fit_t_response_mmrt(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING QUADRATIC RESPONSE
#' out <- fit_t_response_quad(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' 
#' #FITTING TOPT MODEL
#' out <- fit_t_response_topt(pars[pars$Plant == 1,],
#' varnames = list(Par = "V_cmax",
#'                 Tleaf = "Tleaf"))
#' #Return summary
#' summary(out[[1]])
#' 
#' #Look at parameters
#' out[[2]]
#' 
#' #Look at graph
#' out[[3]]
#' }
#' 
#' @rdname fit_t_responses
#' @export
fit_t_response_arrhenius <- function(data,
                                     varnames = list(Par = "Par",
                                                     Tleaf = "Tleaf"),
                                     start = list(Ea = 40000,
                                                  Par25 = 50),
                                     title = NULL){
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Set variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Fit the model
  fit <- nlsLM(data = data, 
               Par ~ Par25 * t_response_arrhenius(Ea,
                                                  Tleaf = Tleaf),
               start = start,
               lower = c(0, 0),
               upper = c(1e10, 10 * max(data$Par)),
               control = nls.control(maxiter = 100))
  
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- fit
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(fit)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ I(output[[2]]$Par25[1] * 
                        (t_response_arrhenius(Tleaf = x,
                                              Ea = output[[2]]$Ea[1]))),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return outputs
  return(output)
}

#' @rdname fit_t_responses
#' @export
fit_t_response_heskel <- function(data,
                                  varnames = list(Par = "Par",
                                                  Tleaf = "Tleaf"),
                                  start = list(a = 1,
                                               b = 1,
                                               c = 1),
                                  title = NULL
)
{
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Fit the model
  fit <- nlsLM(data = data, 
               log(Par) ~ t_response_heskel(a,
                                            b,
                                            c,
                                            Tleaf = Tleaf),
               start = start,
               control = nls.control(maxiter = 100))
  
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- fit
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(fit)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ exp(t_response_heskel(Tleaf = x,
                                          a = output[[2]]$a[1],
                                          b = output[[2]]$b[1],
                                          c = output[[2]]$c[1])),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return outputs
  return(output)
}

#' @rdname fit_t_responses
#' @export
fit_t_response_kruse <- function(data,
                                 varnames = list(Par = "Par",
                                                 Tleaf = "Tleaf"),
                                 start = list(dEa = 1,
                                              Ea_ref = 1,
                                              Par_ref = 1),
                                 title = NULL
)
{
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Calculate temperature in K
  data$T2 <- ((data$Tleaf + 273.15) - 298.15) /
    ((data$Tleaf + 273.15) * 298.15)
  
  #Fit the model
  fit <- nlsLM(data = data, 
               log(Par) ~ t_response_arrhenius_kruse(dEa,
                                                     Ea_ref,
                                                     Par_ref,
                                                     T2 = T2),
               start = start,
               control = nls.control(maxiter = 100))
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- fit
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(fit)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ exp(t_response_arrhenius_kruse(T2 = ((x + 273.15) - 298.15) /
                                                     ((x + 273.15) * 298.15),
                                                   dEa = output[[2]]$dEa[1],
                                                   Ea_ref = output[[2]]$Ea_ref[1],
                                                   Par_ref = output[[2]]$Par_ref[1])),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return outputs
  return(output)
}

#' @rdname fit_t_responses
#' @export
fit_t_response_medlyn <- function(data,
                                  varnames = list(Par = "Par",
                                                  Tleaf = "Tleaf"),
                                  start = list(Ea = 40000,
                                               Hd = 200000,
                                               dS = 650,
                                               Par25 = mean(data$Par)),
                                  setvar = "none",
                                  Hd_set = 200000,
                                  dS_set = 650,
                                  title = NULL){
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Fit both Hd and dS
  if(setvar == "none"){
    #Basically, use Arrhenius curve to feed Ea into Medlyn function start
    #Try approach where you start Hd from 1 to 1000
    #select minimum residual
    model <- nlsLM(data = data, 
                   Par ~ Par25 * t_response_arrhenius(Ea,
                                                      Tleaf = Tleaf),
                   start = list(Par25 = start[[4]],
                                Ea = start[[1]]),
                   lower = c(0, 0),
                   upper = c(1e10, 10 * max(data$Par)),
                   control = nls.control(maxiter = 100)
    )
    
    #Create empty dataframe to fill with 1000 curve fits
    model_fm <- as.data.frame(cbind(rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(varnames$Par[[1]], 1000)))
    #Assign column names
    colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
    
    #Make sure variabel classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    model_full <- list(NULL)
    
    #Run through 1000 instances of the model
    #TryCatch is used to deal with failed fits
    for(i in 1:1000){
      #Fit model
      model_full[[i]] <- tryCatch(nlsLM(data = data, 
                                        Par ~ Par25 * t_response_arrhenius_medlyn(Ea,
                                                                                  Hd,
                                                                                  dS,
                                                                                  Tleaf = Tleaf),
                                        start = list(Ea = coef(model)[[2]],
                                                     Hd = i * 1000,
                                                     dS = dS_set,
                                                     Par25 = coef(model)[[1]]),
                                        lower = c(0, 0, 0, 0),
                                        upper = c(1000000, 2000000, 10000,
                                                  1.5 * max(data$Par)),
                                        control = nls.control(maxiter = 100)),
                                  error = function(e) paste(NA)
      )
      #Extract coefficients
      model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[[1]],
                                 error = function(e) paste(NA))
      model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[[2]],
                                 error = function(e) paste(NA))
      model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[[3]],
                                    error = function(e) paste(NA))
      model_fm$dS[i] <- tryCatch(coef(model_full[[i]])[[4]],
                                 error = function(e) paste(NA))
      model_fm$residual[i] <- tryCatch(sum(abs(model_full[[i]]$m$resid())),
                                       error = function(e) paste(NA))
    }
    
    #Ensure variable classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    
    #Select best model with minimum residuals
    model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
    model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
    
    model_full <- model_full[[as.numeric(rownames(model_fm))]]
    
    #Create empty output list
    output <- list(NULL)
    
    #Add model to output list
    output[[1]] <- model_full
    
    #Add parameter outputs to output list
    output[[2]] <- data.frame(rbind(coef(model_full)))
    output[[2]]$Tleaf <- mean(data$Tleaf)
    
    #Add graph to output list
    output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
      #Add axis labels
      labs(x = expression("Tleaf (Celsius)"),
           y = varnames$Par) +
      #Add title
      ggtitle(label = title) +
      #Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ I(output[[2]]$Par25[1] *
                          t_response_arrhenius_medlyn(Tleaf = x,
                                                      Ea = output[[2]]$Ea[1],
                                                      Hd = output[[2]]$Hd[1],
                                                      dS = output[[2]]$dS[1])),
        size = 2) +
      #Add points
      geom_point(size = 2) +
      #Use clean theme
      theme_bw()
    
    #Name outputs
    names(output) <- c("Model", "Parameters", "Graph")
    
    return(output)
  }
  
  #Just fit dS
  if(setvar == "Hd"){
    #Basically, use Arrhenius curve to feed Ea into Medlyn function start
    #Try approach where you start Hd from 1 to 1000
    #select minimum residual
    model <- nlsLM(data = data, 
                   Par ~ Par25 * t_response_arrhenius(Ea,
                                                      Tleaf = Tleaf),
                   start = list(Par25 = start[[4]],
                                Ea = start[[1]]),
                   lower = c(0, 0),
                   upper = c(1e10, 10 * max(data$Par)),
                   control = nls.control(maxiter = 100)
    )
    
    #Create empty dataframe to fill with 1000 curve fits
    model_fm <- as.data.frame(cbind(rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(varnames$Par[[1]], 1000)))
    #Assign column names
    colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
    
    #Make sure variabel classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    model_full <- list(NULL)
    
    #Run through 1000 instances of the model
    #TryCatch is used to deal with failed fits
    for(i in 1:1000){
      #Fit model
      model_full[[i]] <- tryCatch(nlsLM(data = data, 
                                        Par ~ Par25 * t_response_arrhenius_medlyn(Ea,
                                                                                  Hd = Hd_set,
                                                                                  dS,
                                                                                  Tleaf = Tleaf),
                                        start = list(Ea = coef(model)[[2]],
                                                     dS = i,
                                                     Par25 = coef(model)[[1]]),
                                        lower = c(0, 0, 0),
                                        upper = c(1000000, 10000,
                                                  1.5 * max(data$Par)),
                                        control = nls.control(maxiter = 100)),
                                  error = function(e) paste(NA)
      )
      #Extract coefficients
      model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[[1]],
                                 error = function(e) paste(NA))
      model_fm$Hd[i] <- Hd_set
      model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[[2]],
                                    error = function(e) paste(NA))
      model_fm$dS[i] <- tryCatch(coef(model_full[[i]])[[3]],
                                 error = function(e) paste(NA))
      model_fm$residual[i] <- tryCatch(sum(abs(model_full[[i]]$m$resid())),
                                       error = function(e) paste(NA))
    }
    
    #Ensure variable classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    
    #Select best model with minimum residuals
    model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
    model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
    
    model_full <- model_full[[as.numeric(rownames(model_fm))]]
    
    #Create empty output list
    output <- list(NULL)
    
    #Add model to output list
    output[[1]] <- model_full
    
    #Add parameter outputs to output list
    output[[2]] <- data.frame(rbind(coef(model_full)))
    output[[2]]$Hd <- Hd_set
    output[[2]]$Tleaf <- mean(data$Tleaf)
    
    #Add graph to output list
    output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
      #Add axis labels
      labs(x = expression("Tleaf (Celsius)"),
           y = varnames$Par) +
      #Add title
      ggtitle(label = title) +
      #Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ I(output[[2]]$Par25[1] *
                          t_response_arrhenius_medlyn(Tleaf = x,
                                                      Ea = output[[2]]$Ea[1],
                                                      Hd = output[[2]]$Hd[1],
                                                      dS = output[[2]]$dS[1])),
        size = 2) +
      #Add points
      geom_point(size = 2) +
      #Use clean theme
      theme_bw()
    
    #Name outputs
    names(output) <- c("Model", "Parameters", "Graph")
    
    return(output)
  }
  
  #Just fit Hd
  if(setvar == "dS"){
    #Basically, use Arrhenius curve to feed Ea into Medlyn function start
    #Try approach where you start Hd from 1 to 1000
    #select minimum residual
    model <- nlsLM(data = data, 
                   Par ~ Par25 * t_response_arrhenius(Ea,
                                                      Tleaf = Tleaf),
                   start = list(Par25 = start[[4]],
                                Ea = start[[1]]),
                   lower = c(0, 0),
                   upper = c(1e10, 10 * max(data$Par)),
                   control = nls.control(maxiter = 100)
    )
    
    #Create empty dataframe to fill with 1000 curve fits
    model_fm <- as.data.frame(cbind(rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(0, 1000),
                                    rep(varnames$Par[[1]], 1000)))
    #Assign column names
    colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
    
    #Make sure variabel classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    model_full <- list(NULL)
    
    #Run through 1000 instances of the model
    #TryCatch is used to deal with failed fits
    for(i in 1:1000){
      #Fit model
      model_full[[i]] <- tryCatch(nlsLM(data = data, 
                                        Par ~ Par25 * t_response_arrhenius_medlyn(Ea,
                                                                                  Hd,
                                                                                  dS = dS_set,
                                                                                  Tleaf = Tleaf),
                                        start = list(Ea = coef(model)[[2]],
                                                     Hd = i * 1000,
                                                     Par25 = coef(model)[[1]]),
                                        lower = c(0, 0, 0),
                                        upper = c(1000000, 2000000,
                                                  1.5 * max(data$Par)),
                                        control = nls.control(maxiter = 100)),
                                  error = function(e) paste(NA)
      )
      #Extract coefficients
      model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[[1]],
                                 error = function(e) paste(NA))
      model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[[2]],
                                 error = function(e) paste(NA))
      model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[[3]],
                                    error = function(e) paste(NA))
      model_fm$dS[i] <- dS_set
      model_fm$residual[i] <- tryCatch(sum(abs(model_full[[i]]$m$resid())),
                                       error = function(e) paste(NA))
    }
    
    #Ensure variable classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$dS <- as.double(model_fm$dS)
    model_fm$Par25 <- as.double(model_fm$Par25)
    model_fm$residual <- as.double(model_fm$residual)
    
    #Select best model with minimum residuals
    model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
    model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
    
    model_full <- model_full[[as.numeric(rownames(model_fm))]]
    
    #Create empty output list
    output <- list(NULL)
    
    #Add model to output list
    output[[1]] <- model_full
    
    #Add parameter outputs to output list
    output[[2]] <- data.frame(rbind(coef(model_full)))
    output[[2]]$dS <- dS_set
    output[[2]]$Tleaf <- mean(data$Tleaf)
    
    #Add graph to output list
    output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
      #Add axis labels
      labs(x = expression("Tleaf (Celsius)"),
           y = varnames$Par) +
      #Add title
      ggtitle(label = title) +
      #Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ I(output[[2]]$Par25[1] *
                          t_response_arrhenius_medlyn(Tleaf = x,
                                                      Ea = output[[2]]$Ea[1],
                                                      Hd = output[[2]]$Hd[1],
                                                      dS = output[[2]]$dS[1])),
        size = 2) +
      #Add points
      geom_point(size = 2) +
      #Use clean theme
      theme_bw()
    
    #Name outputs
    names(output) <- c("Model", "Parameters", "Graph")
    
    return(output)
  }
  
}

#' @rdname fit_t_responses
#' @export
fit_t_response_mmrt <- function(data,
                                varnames = list(Par = "Par",
                                                Tleaf = "Tleaf"),
                                start = list(dCp = 1,
                                             dG = 1,
                                             dH = 1),
                                title = NULL
)
{
  
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Fit model
  fit <- nlsLM(data = data, 
               log(Par) ~ t_response_mmrt(dCp,
                                          dG,
                                          dH,
                                          Tleaf = Tleaf),
               start = start,
               control = nls.control(maxiter = 100))
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- fit
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(fit)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ exp(
        t_response_mmrt(Tleaf = x,
                        dCp = output[[2]]$dCp[1],
                        dG = output[[2]]$dG[1],
                        dH = output[[2]]$dH[1])),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return output
  return(output)
}

#' @rdname fit_t_responses
#' @export
fit_t_response_quad <- function(data,
                                varnames = list(Par = "Par",
                                                Tleaf = "Tleaf"),
                                title = NULL){
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Par <- data[, varnames$Par]
  data$Tleaf <- data[, varnames$Tleaf]
  
  #Fit model
  fit <- nlsLM(data = data, Par ~ t_response_heskel(a,
                                                  b,
                                                  c,
                                                  Tleaf = Tleaf),
               start = list(a = 1,
                            b = 2,
                            c = 1),
               control = nls.control(maxiter = 100))
  
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- fit
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(fit)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ (t_response_heskel(Tleaf = x,
                                     a = output[[2]]$a[1],
                                     b = output[[2]]$b[1],
                                     c = output[[2]]$c[1])),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return outputs
  return(output)
}

#' @rdname fit_t_responses
#' @export
fit_t_response_topt <- function(data,
                                varnames = list(Par = "Par",
                                                Tleaf = "Tleaf"),
                                start = list(Ea = 40000,
                                             Par25 = mean(data$Par)
                                ),
                                title = NULL){
  
  #Locally bind variables - avoids notes on check package
  Par <- NULL
  Tleaf <- NULL
  
  #Assign variable names
  data$Tleaf <- data[, varnames$Tleaf]
  data$Par <- data[, varnames$Par]
  
  #Basically, use Arrhenius curve to feed Ea into Topt function start
  #Try approach where you start Hd from 1 to 1000 to ensure model fit
  #select minimum residual
  model <- nlsLM(data = data, 
                 Par ~ Par25 * t_response_arrhenius(Ea,
                                                    Tleaf = Tleaf),
                 start = start,
                 lower = c(0, 0),
                 upper = c(1e10, 10 * max(data$Par)),
                 control = nls.control(maxiter = 100)
  )
  
  #Create empty dataframe to fill with 1000 curve fits
  model_fm <- as.data.frame(cbind(rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(varnames$Par[[1]], 1000)))
  #Assign column names
  colnames(model_fm) <- c("Ea", "Hd", "kopt", "Topt", "residual", 
                          "Parameter")
  
  #Make sure variable classes are appropriate
  model_fm$Ea <- as.double(model_fm$Ea)
  model_fm$Hd <- as.double(model_fm$Hd)
  model_fm$kopt <- as.double(model_fm$kopt)
  model_fm$Topt <- as.double(model_fm$Topt)
  model_fm$residual <- as.double(model_fm$residual)
  model_full <- list(NULL)
  
  #Run through 1000 instances of the model
  #TryCatch is used to deal with failed fits
  for(i in 1:1000){
    #Fit model
    model_full[[i]] <- tryCatch(nlsLM(data = data, 
                                      Par ~ kopt * t_response_arrhenius_topt(Ea,
                                                                             Hd,
                                                                             Topt,
                                                                             Tleaf = Tleaf),
                                      start = list(Ea = coef(model)[[2]],
                                                   Hd = i * 1000,
                                                   kopt = max(data$Par),
                                                   Topt = max(data$Tleaf)),
                                      lower = c(0, 0, 0, 0),
                                      upper = c(1000000, 2000000, max(data$Par) + 1,
                                                max(data$Tleaf) + 1),
                                      control = nls.control(maxiter = 100)),
                                error = function(e) paste(NA)
    )
    #Extract coefficients
    model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[[1]],
                               error = function(e) paste(NA))
    model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[[2]],
                               error = function(e) paste(NA))
    model_fm$kopt[i] <- tryCatch(coef(model_full[[i]])[[3]],
                                 error = function(e) paste(NA))
    model_fm$Topt[i] <- tryCatch(coef(model_full[[i]])[[4]],
                                 error = function(e) paste(NA))
    model_fm$residual[i] <- tryCatch(sum(abs(model_full[[i]]$m$resid())),
                                     error = function(e) paste(NA))
  }
  
  #Ensure variable classes are appropriate
  model_fm$Ea <- as.double(model_fm$Ea)
  model_fm$Hd <- as.double(model_fm$Hd)
  model_fm$kopt <- as.double(model_fm$kopt)
  model_fm$Topt <- as.double(model_fm$Topt)
  model_fm$residual <- as.double(model_fm$residual)
  
  #Select best model with minimum residuals
  model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
  model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
  
  model_full <- model_full[[as.numeric(rownames(model_fm))]]
  
  #Create empty output list
  output <- list(NULL)
  
  #Add model to output list
  output[[1]] <- model_full
  
  #Add parameter outputs to output list
  output[[2]] <- data.frame(rbind(coef(model_full)))
  
  #Add graph to output list
  output[[3]] <- ggplot(data, aes(x = Tleaf, y = Par)) + 
    #Add axis labels
    labs(x = expression("Tleaf (Celsius)"),
         y = varnames$Par) +
    #Add title
    ggtitle(label = title) +
    #Add fitted smoothing function
    geom_smooth(
      method = "lm",
      formula = y ~ I(output[[2]]$kopt[1] *
                        (t_response_arrhenius_topt(Tleaf = x,
                                                   Ea = output[[2]]$Ea[1],
                                                   Hd = output[[2]]$Hd[1],
                                                   Topt = output[[2]]$Topt[1]))),
      size = 2) +
    #Add points
    geom_point(size = 2) +
    #Use clean theme
    theme_bw()
  
  #Name outputs
  names(output) <- c("Model", "Parameters", "Graph")
  
  #Return outputs
  return(output)
}
