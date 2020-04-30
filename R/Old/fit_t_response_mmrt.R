#' Fitting the MMRT temperature response function
#'
#' @param data Dataframe
#' @param varnames Variable names to fit MMRT
#' @param start Starting values for NLS curve fitting
#' @param title Graph title
#'
#' @return fit_t_response_mmrt fits the Macromolecular Rate Theory equation as redefined in
#' Liang et al. 2018, and described in Hobbs et al. 2013.
#' 
#' Hobbs JK, Jiao W, Easter AD, Parker EJ, Schipper LA, Arcus VL.
#' 2013. Change in heat capacity for enzyme catalysis determines
#' temperature dependence of enzyme catalyzed rates. ACS Chemical
#' Biology 8:2388-2393.
#' 
#' Liang LL, Arcus VL, Heskel MA, O'Sullivan OS, Weerasinghe LK,
#' Creek D, Egerton JJG, Tjoelker MG, Atkin OK, Schipper LA. 2018.
#' Macromolecular rate theory (MMRT) provides a thermodynamics
#' rationale to underpin the convergent temperature response in
#' plant leaf respiration. Glob Chang Biol 24:1538-1547
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
#' out <- fit_t_response_mmrt(pars[pars$Plant == 1,],
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
