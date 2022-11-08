#' Running 2-parameter sensitivity analyses
#'
#' @param data Dataframe
#' @param funct Function to use - do not use parentheses
#' @param test1 Input parameter to vary and test
#' @param values1 Values of test1 to use
#' @param test2 Input parameter to vary and test
#' @param values2 Values of test2 to use
#' @param element_out List element to compile
#' @param ... Additional arguments required for the function
#'
#' @return analyze_sensitivity runs a 2-parameter sensitivity analysis.
#' Note that any parameter value combinations that break the input function
#' WILL break this function. For 1-parameter sensitivity analysis, use test1
#' only.
#'
#' @importFrom rlang exec
#' @importFrom rlang :=
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
#' #     to = 40,
#' #     by = 2
#' #   ),
#' #   values2 = seq(
#' #     from = 0.5,
#' #     to = 2,
#' #     by = 0.1
#' #   )
#' # )
#'
#' # Graph V_cmax
#' # ggplot(pars, aes(x = gamma_star25, y = g_mc25, z = V_cmax)) +
#' #   geom_tile(aes(fill = V_cmax)) +
#' #   labs(
#' #     x = expression(Gamma * "*"[25] ~ "(" * mu * mol ~ mol^
#' #       {
#' #         -1
#' #       } * ")"),
#' #     y = expression(g[m][25] ~ "(" * mu * mol ~ m^{
#' #       -2
#' #     } ~ s^{
#' #       -1
#' #     } ~ Pa^
#' #       {
#' #         -1
#' #       } * ")")
#' #   ) +
#' #   scale_fill_distiller(palette = "Greys") +
#' #   geom_contour(colour = "Black", size = 1) +
#' #   theme_bw()
#' # }
#'
analyze_sensitivity <- function(data,
                                funct,
                                test1 = NA,
                                values1,
                                test2 = NA,
                                values2,
                                element_out = 1,
                                ...) {
  # Create an empty list for ACi fits
  fits <- list(NULL)

  # Next loops through values depending on whether there are
  # two input parameters or one.
  # Note that these loops generalize the arguments and functions
  if (!is.na(test2)) {
    # Start progress bar
    pb <- txtProgressBar(min = 0, max = length(values2) *
      length(values1), style = 3)
    # Loop through values of test1 and test2
    for (j in seq_len(length(values2))) {
      for (i in seq_len(length(values1))) {
        fits[[i + (j - 1) * length(values1)]] <- exec(funct,
          data = data,
          !!test1 := values1[i],
          !!test2 := values2[j],
          ...
        )
        # Set progress bar
        setTxtProgressBar(pb, i + (j - 1) * length(values1))
      }
    }
  } else {
    # Start progress bar
    pb <- txtProgressBar(min = 0, max = length(values1), style = 3)
    for (i in seq_len(length(values1))) {
      fits[[i]] <- exec(funct,
        data = data,
        !!test1 := values1[i],
        ...
      )
      # Set progress bar
      setTxtProgressBar(pb, i)
    }
  }
  # Create empty list for parameter outputs
  pars <- vector("list", length(fits))

  # Compile parameter outputs
  # Main challenge here when using out-of-package functions:
  # Output style may vary, making this component behave
  # unexpectedly
  for (i in seq_along(fits)) {
    pars[[i]] <- fits[[i]][[element_out]]
  }
  # Convert parameter outputs to dataframe
  pars <- do.call("bind_rows", pars)
  # Return parameters as output
  return(pars)
}
