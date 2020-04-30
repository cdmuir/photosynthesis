#' Fitting a peaked Arrhenius function
#'
#' @param data Dataframe
#' @param varnames Variable names to set parameters to fit
#' @param start Starting values for curve fitting
#' @param setvar Set a variable to constant? One of "none", "Hd" or "dS"
#' @param Hd_set Value to set for Hd in J/mol
#' @param dS_set Value to set for dS in J/mol
#' @param title Graph title
#'
#' @return fit_t_response_medlyn fits the Medlyn et al. 2002 temperature
#' response model to data.
#' 
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom minpack.lm nlsLM
#' @importFrom stats nls.control
#' @export
#' 
#' @examples \dontrun{
#' out <- fit_t_response_medlyn(pars[pars$Plant == 1,],
#' varnames = list(Par = "Vcmax",
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
#' }
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
