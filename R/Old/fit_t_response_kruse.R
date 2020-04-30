#' Fitting a peaked Arrhenius model
#'
#' @param data Dataframe
#' @param varnames Variable names to fit
#' @param start Starting values for NLS curve fitting
#' @param title Graph title
#'
#' @return fit_t_response_kruse fits the Kruse et al. 2008 model of
#' temperature responses
#' 
#' Kruse J, Adams MA. 2008. Three parameters comprehensively describe
#' the temperature response of respiratory oxygen reduction. Plant 
#' Cell Environ 31:954-967
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
#' out <- fit_t_response_kruse(pars[pars$Plant == 1,],
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
