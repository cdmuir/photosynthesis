#' Fitting hydraulic vulnerability curves
#'
#' @param data Dataframe
#' @param varnames List of variable names. varnames = list(psi = "psi",
#' PLC = "PLC") where psi is water potential in MPa, and PLC is percent
#' loss conductivity.
#' @param title Title for the output graph
#' @param start_weibull starting values for the nls fitting routine
#' for the Weibull curve
#'
#' @return fit_hydra_vuln_curve fits a sigmoidal function (Pammenter & Van der
#' Willigen, 1998) linearized according to Ogle et al. (2009). Output is a list
#' containing the sigmoidal model in element 1 and Weibull model in element 4,
#' the fit parameters with 95% confidence interval for both models are in
#' element 2, and hydraulic parameters in element 3 (including P25, P50, P88,
#' P95, S50, Pe, Pmax, DSI). Px (25 to 95): water potential at which x% of
#' conductivity is lost. S50: slope at 50% loss of conductivity. Pe: air
#' entry point. Pmax: hydraulic failure threshold. DSI: drought stress interval.
#' Element 5 is a graph showing the fit, P50, Pe, and Pmax.
#'
#' @references
#' Ogle K, Barber JJ, Willson C, Thompson B. 2009. Hierarchical statistical
#' modeling of xylem vulnerability to cavitation. New Phytologist 182:541-554
#'
#' Pammenter NW, Van der Willigen CV. 1998. A mathematical and statistical
#' analysis of the curves illustrating vulnerability of xylem to cavitation.
#' Tree Physiology 18:589-593
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom stats confint
#' @importFrom stats deriv
#'
#' @export
#' @examples
#' \donttest{
#' # Read in data
#' data <- read.csv(system.file("extdata", "hydraulic_vulnerability.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit hydraulic vulnerability curve
#' fit <- fit_hydra_vuln_curve(data[data$Tree == 4 & data$Plot == "Control", ],
#'   varnames = list(
#'     psi = "P",
#'     PLC = "PLC"
#'   ),
#'   title = "Control 4"
#' )
#'
#' # Return Sigmoidal model summary
#' summary(fit[[1]])
#'
#' # Return Weibull model summary
#' summary(fit[[4]])
#'
#' # Return model parameters with 95\% confidence intervals
#' fit[[2]]
#'
#' # Return hydraulic parameters
#' fit[[3]]
#'
#' # Return graph
#' fit[[5]]
#'
#' # Fit many curves
#' fits <- fit_many(
#'   data = data,
#'   varnames = list(
#'     psi = "P",
#'     PLC = "PLC"
#'   ),
#'   group = "Tree",
#'   funct = fit_hydra_vuln_curve
#' )
#'
#' # To select individuals from the many fits
#' # Return model summary
#' summary(fits[[1]][[1]]) # Returns model summary
#'
#' # Return sigmoidal model output
#' fits[[1]][[2]]
#'
#' # Return hydraulic parameters
#' fits[[1]][[3]]
#'
#' # Return graph
#' fits[[1]][[5]]
#'
#' # Compile parameter outputs
#' pars <- compile_data(
#'   data = fits,
#'   output_type = "dataframe",
#'   list_element = 3
#' )
#'
#' # Compile graphs
#' graphs <- compile_data(
#'   data = fits,
#'   output_type = "list",
#'   list_element = 5
#' )
#' }
fit_hydra_vuln_curve <- function(data,
                                 varnames = list(
                                   psi = "psi",
                                   PLC = "PLC"
                                 ),
                                 start_weibull = list(
                                   a = 2,
                                   b = 2
                                 ),
                                 title = NULL) {
  # Locally bind variables - avoids notes on check package
  psi <- NULL
  PLC <- NULL
  # Assign variable names
  data$psi <- data[, varnames$psi]
  data$PLC <- data[, varnames$PLC]
  # Prepare y variable for sigmoidal function
  data$H_log <- log(100 / data$PLC - 1)
  # Prepare y variable for Weibull function
  data$K.Kmax <- (1 - data$PLC / 100)
  # Generate empty list for data outputs
  fit_out <- list(NULL)
  # Starting with the sigmoidal model
  # Fit model, remove any infinite values (e.g. at P = 0)
  fit_out[[1]] <- lm(H_log ~ psi, data[data$H_log < Inf, ])
  # Extract model parameter values
  fit_out[[2]] <- data.frame(c(coef(fit_out[[1]])[1], coef(fit_out[[1]])[2]))
  # Note that in model, the intercept is - a * b
  fit_out[[2]][1, ] <- fit_out[[2]][1, ] / -fit_out[[2]][2, ]
  # Assign row names
  rownames(fit_out[[2]]) <- c("b", "a")
  # Assign parameter names
  fit_out[[2]]$Parameter <- rownames(fit_out[[2]])
  # Assign curve name
  fit_out[[2]]$Curve <- "Sigmoidal"
  # Assign column names
  colnames(fit_out[[2]]) <- c(
    "Value",
    "Parameter", "Curve"
  )
  # Create dataframe for calculated parameters
  fit_out[[3]] <- as.data.frame(rbind(1:8))
  # Add column names
  colnames(fit_out[[3]]) <- c(
    "P25", "P50", "P88", "P95",
    "S50", "Pe", "Pmax", "DSI"
  )
  # Assign a and b values to minimize typing
  a <- fit_out[[2]]$Value[2]
  b <- fit_out[[2]]$Value[1]
  # Calculate hydraulic parameters
  fit_out[[3]]$P25 <- log(1 / (0.25) - 1) / (a) + b
  fit_out[[3]]$P50 <- log(1 / (0.5) - 1) / (a) + b
  fit_out[[3]]$P88 <- log(1 / (0.88) - 1) / (a) + b
  fit_out[[3]]$P95 <- log(1 / (0.95) - 1) / (a) + b
  # To get slope, we need to take derivative of model and calculate at P50
  XX <- fit_out[[3]]$P50
  dWpsi <- deriv(~ 100 / (1 + exp(a * (XX - b))), "XX")
  derivSoln <- eval(dWpsi)
  # This extracts the slope at P50
  fit_out[[3]]$S50 <- as.numeric(attributes(derivSoln)$gradient)
  # Next calculate the air entry point
  yint <- 50 - (fit_out[[3]]$S50 * fit_out[[3]]$P50)
  fit_out[[3]]$Pe <- -yint / fit_out[[3]]$S50
  # Calculate the hydraulic failure threshold
  fit_out[[3]]$Pmax <- (100 - yint) / fit_out[[3]]$S50
  # Calculate the drought stress interval
  fit_out[[3]]$DSI <- fit_out[[3]]$Pmax - fit_out[[3]]$Pe
  # Add curve name
  fit_out[[3]]$Curve <- "Sigmoidal"
  # Remove a and b values
  remove(a)
  remove(b)
  # Moving on to Weibull model
  fit_out[[4]] <- nlsLM(
    data = data,
    K.Kmax ~ exp(-((psi / a)^b)),
    start = start_weibull
  )
  # Extract model parameter values
  fit_out[[5]] <- data.frame(c(coef(fit_out[[4]])[2], coef(fit_out[[4]])[1]))
  # Assign parameter names
  fit_out[[5]]$Parameter <- rownames(fit_out[[5]])
  # Assign curve name
  fit_out[[5]]$Curve <- "Weibull"
  # Assign column names
  colnames(fit_out[[5]]) <- c(
    "Value",
    "Parameter", "Curve"
  )
  # Merge model parameters to reduce size of list
  fit_out[[2]] <- bind_rows(fit_out[[2]], fit_out[[5]])
  # Create dataframe for calculated parameters
  fit_out[[5]] <- as.data.frame(rbind(1:8))
  # Add column names
  colnames(fit_out[[5]]) <- c(
    "P25", "P50", "P88", "P95",
    "S50", "Pe", "Pmax", "DSI"
  )
  # Assign a and b values to minimize typing
  a <- fit_out[[2]]$Value[4]
  b <- fit_out[[2]]$Value[3]
  # Calculate hydraulic parameters
  fit_out[[5]]$P25 <- (-log(1 - 25 / 100))^(1 / b) * a
  fit_out[[5]]$P50 <- (-log(1 - 50 / 100))^(1 / b) * a
  fit_out[[5]]$P88 <- (-log(1 - 88 / 100))^(1 / b) * a
  fit_out[[5]]$P95 <- (-log(1 - 95 / 100))^(1 / b) * a
  # To get slope, we need to take derivative of model and calculate at P50
  dWpsi <- deriv(~ (1 - exp(-(XX / a)^b)) * 100, "XX")
  XX <- fit_out[[5]]$P50
  derivSoln <- eval(dWpsi)
  # This extracts the slope at P50
  fit_out[[5]]$S50 <- as.numeric(attributes(derivSoln)$gradient)
  # Next calculate the air entry point
  yint <- 50 - (fit_out[[5]]$S50 * fit_out[[5]]$P50)
  fit_out[[5]]$Pe <- -yint / fit_out[[5]]$S50
  # Calculate the hydraulic failure threshold
  fit_out[[5]]$Pmax <- (100 - yint) / fit_out[[5]]$S50
  # Calculate the drought stress interval
  fit_out[[5]]$DSI <- fit_out[[5]]$Pmax - fit_out[[5]]$Pe
  # Add curve name
  fit_out[[5]]$Curve <- "Weibull"
  # Combine output variables to reduce size of list
  fit_out[[3]] <- rbind(fit_out[[3]], fit_out[[5]])
  # Generates plot of hydraulic vulnerability curve
  fit_out[[5]] <- ggplot(data, aes(x = psi, y = PLC)) +
    ggtitle(label = title) +
    geom_vline(
      xintercept = fit_out[[3]]$P50[1],
      linewidth = 2,
      colour = "Blue"
    ) +
    geom_vline(
      xintercept = fit_out[[3]]$Pe[1],
      linewidth = 2,
      colour = "Blue"
    ) +
    geom_vline(
      xintercept = fit_out[[3]]$Pmax[1],
      linewidth = 2,
      colour = "Blue"
    ) +
    geom_vline(
      xintercept = fit_out[[3]]$P50[2],
      linewidth = 2,
      colour = "Orange",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = fit_out[[3]]$Pe[2],
      linewidth = 2,
      colour = "Orange",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = fit_out[[3]]$Pmax[2],
      linewidth = 2,
      colour = "Orange",
      linetype = "dashed"
    ) +
    geom_smooth(
      method = "lm", aes(
        x = psi, y = PLC,
        colour = "Blue"
      ), show.legend = TRUE,
      formula = y ~ I(100 /
        (1 + exp(fit_out[[2]]$Value[2] *
          (x - fit_out[[2]]$Value[1])))),
      linewidth = 2
    ) +
    geom_smooth(
      method = "lm", aes(
        x = psi, y = PLC,
        colour = "DarkOrange"
      ), show.legend = TRUE,
      formula = y ~
      I((1 - exp(-((x / fit_out[[2]]$Value[4])^
        fit_out[[2]]$Value[3]))) * 100),
      linewidth = 2
    ) +
    geom_point(size = 2, aes(colour = "Black")) +
    labs(y = "PLC (%)", x = "Water Potential (-MPa)") +
    scale_colour_manual(
      values = c("Black", "Blue", "Orange"),
      labels = c("Data", "Sigmoidal", "Weibull")
    ) +
    annotate("text",
      label = "Left to Right:
             Pe > P50 > Pmax",
      x = 0.5, y = 75
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
  # Assign names to elements in the output list
  names(fit_out) <- c(
    "Sig_Model", "Model_Parameters", "Model_Px Values",
    "Wei_Model", "Graph"
  )
  # Return output list
  return(fit_out)
}
