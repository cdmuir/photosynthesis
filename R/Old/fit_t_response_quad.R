#' Fitting a 2nd order polynomial
#'
#' @param data Dataframe
#' @param varnames Variable names to fit
#' @param title Graph title
#'
#' @return fit_t_response_quad is used to fit a 2nd order polynomial to
#' temperature response data
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
#' out <- fit_t_response_quad(pars[pars$Plant == 1,],
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
  fit <- nlsLM(data = data, Par ~ t_response_quad(a,
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
      formula = y ~ (t_response_quad(Tleaf = x,
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
