#' Fitting pressure-volume curves
#'
#' @param data Dataframe
#' @param varnames Variable names. varnames = list(psi = "psi", mass =
#' "mass", leaf_mass = "leaf_mass", bag_mass = "bag_mass", leaf_area =
#' "leaf_area") where psi is leaf water potential in MPa, mass is the
#' weighed mass of the bag and leaf in g, leaf_mass is the mass of the
#' leaf in g, bag_mass is the mass of the bag in g, and leaf_area is
#' the area of the leaf in cm2.
#' @param title Graph title
#'
#' @return fit_PV_curve fits pressure-volume curve data to determine:
#' SWC: saturated water content per leaf mass (g H2O g leaf dry mass ^ -1),
#' PI_o: osmotic potential at full turgor (MPa), psi_TLP: leaf water
#' potential at turgor loss point (TLP) (MPa), RWC_TLP: relative water
#' content at TLP (%), eps: modulus of elasticity at full turgor (MPa),
#' C_FT: relative capacitance at full turgor (MPa ^ -1), C_TLP: relative
#' capacitance at TLP (MPa ^ -1), and C_FTStar: absolute capacitance per
#' leaf area (g m ^ -2 MPa ^ -1). Element 1 of the output list contains
#' the fitted parameters, element 2 contains the water-psi graph, and
#' element 3 contains the 1/psi-100-RWC graph.
#'
#' @references
#' Koide RT, Robichaux RH, Morse SR, Smith CM. 2000. Plant water status,
#' hydraulic resistance and capacitance. In: Plant Physiological Ecology:
#' Field Methods and Instrumentation (eds RW Pearcy, JR Ehleringer, HA
#' Mooney, PW Rundel), pp. 161-183. Kluwer, Dordrecht, the Netherlands
#'
#' Sack L, Cowan PD, Jaikumar N, Holbrook NM. 2003. The 'hydrology' of
#' leaves: co-ordination of structure and function in temperate woody
#' species. Plant, Cell and Environment, 26, 1343-1356
#'
#' Tyree MT, Hammel HT. 1972. Measurement of turgor pressure and water
#' relations of plants by pressure bomb technique. Journal of Experimental
#' Botany, 23, 267
#'
#' @importFrom ggplot2 geom_hline
#' @export
#'
#' @examples
#' \donttest{
#' # Read in data
#' data <- read.csv(system.file("extdata", "PV_curve.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit one PV curve
#' fit <- fit_PV_curve(data[data$ID == "L2", ],
#'   varnames = list(
#'     psi = "psi",
#'     mass = "mass",
#'     leaf_mass = "leaf_mass",
#'     bag_mass = "bag_mass",
#'     leaf_area = "leaf_area"
#'   )
#' )
#'
#' # See fitted parameters
#' fit[[1]]
#'
#' # Plot water mass graph
#' fit[[2]]
#'
#' # Plot PV Curve
#' fit[[3]]
#'
#' # Fit all PV curves in a file
#' fits <- fit_many(data,
#'   group = "ID",
#'   funct = fit_PV_curve,
#'   varnames = list(
#'     psi = "psi",
#'     mass = "mass",
#'     leaf_mass = "leaf_mass",
#'     bag_mass = "bag_mass",
#'     leaf_area = "leaf_area"
#'   )
#' )
#'
#' # See parameters
#' fits[[1]][[1]]
#'
#' # See water mass - water potential graph
#' fits[[1]][[2]]
#'
#' # See PV curve
#' fits[[1]][[3]]
#'
#' # Compile parameter outputs
#' pars <- compile_data(
#'   data = fits,
#'   output_type = "dataframe",
#'   list_element = 1
#' )
#'
#' # Compile the water mass - water potential graphs
#' graphs1 <- compile_data(
#'   data = fits,
#'   output_type = "list",
#'   list_element = 2
#' )
#'
#' # Compile the PV graphs
#' graphs2 <- compile_data(
#'   data = fits,
#'   output_type = "list",
#'   list_element = 3
#' )
#' }
fit_PV_curve <- function(data,
                         varnames = list(
                           psi = "psi",
                           mass = "mass",
                           leaf_mass = "leaf_mass",
                           bag_mass = "bag_mass",
                           leaf_area = "leaf_area"
                         ),
                         title = NULL) {
  # Locally bind variables
  inv_psi <- NULL
  inv_psi_pred <- NULL
  leaf_water <- NULL
  psi <- NULL
  psi_pred <- NULL
  `100-RWC` <- NULL
  # Set variable names
  data$psi <- data[, varnames$psi]
  data$mass <- data[, varnames$mass]
  data$leaf_mass <- data[, varnames$leaf_mass]
  data$bag_mass <- data[, varnames$bag_mass]
  data$leaf_area <- data[, varnames$leaf_area]
  # Generate list for outputs
  output <- list(NULL)
  # Generate dataframe for outputs within list
  output[[1]] <- as.data.frame(rbind(1:8))
  colnames(output[[1]]) <- c(
    "SWC",
    "PI_o",
    "psi_TLP",
    "RWC_TLP",
    "eps",
    "C_FT",
    "C_TLP",
    "C_FTStar"
  )
  # Calculate inverse water potential for calculations
  data$inv_psi <- -1 / data$psi
  # Assign single value for leaf mass, bag mass, and leaf area
  # First we assign NULL values to make sure the variable is
  # locally bound to the function and not integrated into the
  # global environment
  leaf_mass <- NULL
  bag_mass <- NULL
  leaf_area <- NULL
  leaf_mass <- data$leaf_mass[1]
  bag_mass <- data$bag_mass[1]
  leaf_area <- data$leaf_area[1]
  # Calculate leaf water
  data$leaf_water <- data$mass - leaf_mass - bag_mass
  # Create empty list for regressions
  water_fit <- list(NULL)
  # Create vector of r-squared values for model selection
  # Length is -2 because regression needs > 2 points
  Rsq <- c(1:(length(data$mass) - 2))
  # This regression needs to be from beginning to end of linear water loss
  # Needs at least 3 points, hence i starting at 3, but cap at 5 to avoid
  # issues if the first three points are not very linear
  for (i in 3:length(data$mass)) {
    water_fit[[i - 2]] <- lm(psi ~ leaf_water, data[1:i, ])
    water_fit[[i - 2]]$Rsq <- summary(water_fit[[i - 2]])$r.squared
    Rsq[i - 2] <- summary(water_fit[[i - 2]])$r.squared
  }
  # Need to select best fit based on r-squared
  for (i in 1:3) {
    if (water_fit[[i]]$Rsq == max(Rsq[1:3])) {
      bestfit <- water_fit[[i]]
    }
  }
  # Calculate saturated water content
  # This is only for calulating other parameters
  SWC <- -coef(bestfit)[1] / coef(bestfit)[2]
  # Calculate saturated water content on leaf mass basis
  output[[1]]$SWC <- SWC / leaf_mass
  # Calculate relative water content
  data$RWC <- data$leaf_water / SWC
  # Convert RWC to percent and 100 - RWC
  data$RWC_percent <- 100 * data$RWC
  data$`100-RWC` <- 100 - data$RWC_percent
  # Generate predicted psi for psi-water plot
  data$psi_pred <- coef(bestfit)[[2]] * data$leaf_water + coef(bestfit)[[1]]
  # Generate psi water plot - lets you see points used for regression
  output[[2]] <- ggplot(data, aes(x = leaf_water, y = psi)) +
    labs(y = expression(Psi[leaf] ~ "(MPa)", x = "Mass of water (g)")) +
    geom_hline(yintercept = 0) +
    geom_line(aes(y = psi_pred), colour = "Grey", size = 2) +
    geom_point(size = 2) +
    theme_bw()
  # Remove bestfit information to avoid code complications
  bestfit <- NULL
  # Generate empty list for predicting turgor loss point
  psi_fit <- list(NULL)
  # Generate r-squared vector
  Rsq <- c(1:(length(data$mass) - 4))
  # Run regressions, ensuring that there is a minimum of 3 points
  # i starts at 3 to avoid first 2 points where large changes in psi
  # can occur. Also finds cutoff observation for other calculations
  for (i in 3:(length(data$inv_psi) - 2)) {
    psi_fit[[i - 2]] <- lm(
      inv_psi ~ `100-RWC`,
      data[(length(data$inv_psi) - i):length(data$inv_psi), ]
    )
    psi_fit[[i - 2]]$Rsq <- summary(psi_fit[[i - 2]])$r.squared
    psi_fit[[i - 2]]$Obs_cut <- i
    Rsq[i - 2] <- summary(psi_fit[[i - 2]])$r.squared
  }
  # Find best model based on r-squared
  for (i in 1:length(psi_fit)) {
    if (psi_fit[[i]]$Rsq == max(Rsq)) {
      bestfit <- psi_fit[[i]]
    }
  }
  # Calculate psi and RWC at turgor loss point
  output[[1]]$psi_TLP <- data[bestfit$Obs_cut, ]$psi
  output[[1]]$RWC_TLP <- data[bestfit$Obs_cut, ]$RWC * 100
  # Calculate osmotic potential at full turgor
  output[[1]]$PI_o <- -1 / coef(bestfit)[1]
  # Caclulate osmotic water potential
  data$psi_o <- -1 / (coef(bestfit)[1] + coef(bestfit)[2] * data$`100-RWC`)
  data$psi_p <- data$psi - data$psi_o
  # Calculate modulus of elasticity at full turgor
  output[[1]]$eps <- coef(lm(psi_p ~ RWC, data[1:bestfit$Obs_cut, ]))[2]
  # Calculate relative capacitance at full turgor
  # Include cutoff observations and points above cutoff
  output[[1]]$C_FT <- coef(lm(RWC ~ psi, data[1:bestfit$Obs_cut, ]))[2]
  # Calculate relative capacitance at turgor loss point
  # Include cutoff observations and points below cutoff
  output[[1]]$C_TLP <- coef(lm(
    RWC ~ psi,
    data[bestfit$Obs_cut:length(data$psi), ]
  ))[2]
  # Calculate absolute capacitance per area at full turgor
  output[[1]]$C_FTStar <- output[[1]]$C_FT * SWC / 18 / (leaf_area / 10000)
  # Calculate predicted inverse psi for graphing
  data$inv_psi_pred <- coef(bestfit)[[2]] * data$`100-RWC` + coef(bestfit)[[1]]
  # Graph the turgor loss point graph
  output[[3]] <- ggplot(data, aes(x = `100-RWC`, y = inv_psi)) +
    ggtitle(label = title) +
    labs(y = expression("1 / " * Psi ~ "(MP" * a^{
      -1
    } * ")")) +
    geom_line(aes(y = inv_psi_pred), size = 3, colour = "Grey") +
    geom_line(size = 1, colour = "Black") +
    geom_point(size = 4) +
    theme_bw()
  # Add names to output list
  names(output) <- c(
    "PV Parameters",
    "Water Mass - Water Potential Graph",
    "TLP Graph"
  )
  # Return output
  return(output)
}
