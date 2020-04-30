#' Fitting a peaked Arrhenius model
#'
#' @param data Dataframe
#' @param varnames Variable names to fit
#' @param start Starting values for NLS curve fitting
#' @param title Graph title
#'
#' @return fit_t_response_topt fits the peaked Arrhenius model from
#' Medlyn et al. 2002 with kopt and Topt.
#' 
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
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
#' out <- fit_t_response_topt(pars[pars$Plant == 1,],
#' varnames = list(Par = "Vcmax",
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
