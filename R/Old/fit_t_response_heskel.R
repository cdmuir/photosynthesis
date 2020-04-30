#' Fitting the Heskel temperature response model
#'
#' @param data Dataframe
#' @param varnames List of variable names to fit
#' @param start Starting values for NLS curve fitting
#' @param title Graph title
#'
#' @return fit_t_response_heskel fits a polynomial to log-transformed temperature responses
#' based on Heskel et al. 2016
#' 
#' Heskel MA, O'Sullivan OS, Reich PB, Tjoelker MG, Weerasinghe LK,
#' Penillard A, Egerton JJG, Creek D, Bloomfield KJ, Xiang J, Sinca F,
#' Stangl ZR, la Torre AM, Griffin KL, Huntingford C, Hurry V, Meir P,
#' Turnbull MH, Atkin OK. 2016. Convergence in the temperature response
#' of leaf respiration across biomes and plant functional types. PNAS
#' 113:3832-3837
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
#' out <- fit_t_response_heskel(pars[pars$Plant == 1,],
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
