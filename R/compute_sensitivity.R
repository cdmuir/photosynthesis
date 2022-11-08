#' Computing measures of sensitivity
#'
#' @param data Dataframe with output from sensitivity_analysis()
#' @param varnames Variable names
#' @param test1_ref Reference value for parameter
#' @param test2_ref Reference value for parameter
#'
#' @return compute_sensitivity calculates two sets of sensitivity measures:
#' parameter effect (Bauerle et al., 2014), and control coefficient (Capaldo &
#' Pandis, 1997). This function is useful in determining how much a given
#' input (assumed or otherwise) can affect the model output and conclusions.
#' Particularly useful if a given parameter is unknown during a fitting or
#' modeling process.
#'
#' @references
#' Bauerle WL, Daniels AB, Barnard DM. 2014. Carbon and water flux responses to
#' physiology by environment interactions: a sensitivity analysis of variation
#' in climate on photosynthetic and stomatal parameters. Climate Dynamics 42:
#' 2539-2554.
#'
#' Capaldo KP, Pandis SN 1997. Dimethylsulfide chemistry in the remote marine
#' atmosphere: evaluation and sensitivity analysis of available mechanisms.
#' J Geophys Res 102:23251-23267
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @export
#'
#' @examples
#' \donttest{
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Define a grouping factor based on light intensity to split the ACi
#' # curves
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#'
#' # Convert leaf temperature to K
#' data$T_leaf <- data$Tleaf + 273.15
#'
#' # Run a sensitivity analysis on gamma_star and mesophyll conductance
#' # at 25 Celsius for one individual curve
#' # pars <- analyze_sensitivity(
#' #   data = data[data$Q_2 == 1500, ],
#' #   funct = fit_aci_response,
#' #   varnames = list(
#' #     A_net = "A",
#' #     T_leaf = "T_leaf",
#' #     C_i = "Ci",
#' #     PPFD = "Qin"
#' #   ),
#' #   useg_mct = TRUE,
#' #   test1 = "gamma_star25",
#' #   element_out = 1,
#' #   test2 = "g_mc25",
#' #   fitTPU = TRUE,
#' #   Ea_gamma_star = 0,
#' #   Ea_g_mc = 0,
#' #   values1 = seq(
#' #     from = 20,
#' #     to = 60,
#' #     by = 2
#' #   ),
#' #   values2 = seq(
#' #     from = 0.2,
#' #     to = 2,
#' #     by = 0.1
#' #   )
#' # )
#' # Compute measures of sensitivity
#' # par2 <- compute_sensitivity(
#' #   data = pars,
#' #   varnames = list(
#' #     Par = "V_cmax",
#' #     test1 = "gamma_star25",
#' #     test2 = "g_mc25"
#' #   ),
#' #   test1_ref = 42,
#' #   test2_ref = 1
#' # )
#' # # Plot control coefficients
#' # ggplot(par2, aes(y = CE_gamma_star25, x = CE_g_mc25, colour = V_cmax)) +
#' #   geom_point() +
#' #   theme_bw()
#' # # Note that in this case a missing point appears due to an infinity
#' }
compute_sensitivity <- function(data,
                                varnames = list(
                                  Par = "Par",
                                  test1 = "test1",
                                  test2 = "test2"
                                ),
                                test1_ref,
                                test2_ref) {
  # Set variable names
  data$Par <- data[, varnames$Par]
  data$test1 <- data[, varnames$test1]
  data$test2 <- data[, varnames$test2]
  # Calculate parameter effect (PE) of one input per each instance of
  # the other input. Therefore need to split data relative to one variable,
  # calculate PE, merge data, then split by other variable and repeat
  # Split data by variable 2
  data <- split(data, data$test2)
  # Start progress bar
  pb <- txtProgressBar(min = 0, max = length(data), style = 3)
  # Calculate parameter effect
  for (i in 1:length(data)) {
    data[[i]]$PE_test1 <- abs(data[[i]][data[[i]]$test1 ==
      max(data[[i]]$test1), ]$Par -
      data[[i]][data[[i]]$test1 ==
        min(data[[i]]$test1), ]$Par) /
      mean(data[[i]]$Par) * 100
    # Set progress bar
    setTxtProgressBar(pb, i)
  }
  # Bind back to dataframe
  data <- do.call("rbind", data)
  # Split data by variable 1
  data <- split(data, data$test1)
  # Start progress bar
  pb <- txtProgressBar(min = 0, max = length(data), style = 3)
  # Calculate parameter effect
  for (i in 1:length(data)) {
    data[[i]]$PE_test2 <- abs(data[[i]][data[[i]]$test2 ==
      max(data[[i]]$test2), ]$Par -
      data[[i]][data[[i]]$test2 ==
        min(data[[i]]$test2), ]$Par) /
      mean(data[[i]]$Par) * 100
    # Set progress bar
    setTxtProgressBar(pb, i)
  }
  # Bind back to dataframe
  data <- do.call("rbind", data)

  # Calculate control coefficients. In this case, we are deriving it numerically
  # Need reference point in the entire parameter space, this is test1_ref and
  # test2_ref. Calculations from Capaldo & Pandis 1997.
  data$CE_test1 <- NA
  data$CE_test2 <- NA
  for (i in 1:nrow(data)) {
    data$CE_test1[i] <- (log(data$Par[i]) - log(data[data$test1 == test1_ref &
      data$test2 == test2_ref, ]$Par)) /
      (log(data$test1[i]) - log(data[data$test1 == test1_ref &
        data$test2 == test2_ref, ]$test1))
    data$CE_test2[i] <- (log(data$Par[i]) - log(data[data$test1 == test1_ref &
      data$test2 == test2_ref, ]$Par)) /
      (log(data$test2[i]) - log(data[data$test1 == test1_ref &
        data$test2 == test2_ref, ]$test2))
  }

  # Name output columns based on selected variable name
  for (i in 1:ncol(data)) {
    if (colnames(data)[i] == "PE_test1") {
      colnames(data)[i] <- paste0("PE_", varnames$test1)
    }
    if (colnames(data)[i] == "PE_test2") {
      colnames(data)[i] <- paste0("PE_", varnames$test2)
    }
    if (colnames(data)[i] == "CE_test1") {
      colnames(data)[i] <- paste0("CE_", varnames$test1)
    }
    if (colnames(data)[i] == "CE_test2") {
      colnames(data)[i] <- paste0("CE_", varnames$test2)
    }
  }
  # Return dataframe
  return(data)
}
