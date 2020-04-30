#' Fitting the Arrhenius temperature response
#'
#' @param data Dataframe
#' @param varnames Variable names for the parameter to fit and leaf temperature
#' @param start List of starting values for NLS curve fitting
#' @param title Title for the graph
#'
#' @return fit_t_response_arrhenius fits an Arrhenius temperature response
#' to data
#' 
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
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
#' out <- fit_t_response_arrhenius(pars[pars$Plant == 1,],
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
