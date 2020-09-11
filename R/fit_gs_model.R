#' Fitting stomatal conductance models
#'
#' @param data Dataframe
#' @param varnames Variable names
#'
#' For the Ball-Berry model: varnames = list(A_net = "A_net", C_air = "C_air",
#' g_sw = "g_sw", RH = "RH") where A_net is net CO2 assimilation, C_air is CO2
#' concentration at the leaf surface in umol mol-1, g_sw is stomatal
#' conductance to H2O, and RH is relative humidity as a proportion.
#'
#' For the Leuning model: varnames = list(A_net = "A_net", C_air = "C_air",
#' g_sw = "g_sw", VPD = "VPD") where A_net is net CO2 assimilation, C_air is
#' CO2 concentration at the leaf surface in umol mol-1, g_sw is stomatal
#' conductance to H2O, and VPD is leaf to air vapor pressure deficit in kPa.
#'
#' For the Medlyn et al. 2011 models: varnames = list(A_net = "A_net",
#' C_air = "C_air", g_sw = "g_sw", VPD = "VPD") where A_net is net CO2
#' assimilation, C_air is CO2 concentration at the leaf surface in umol mol-1,
#' g_sw is stomatal conductance to H2O, and VPD is leaf to air vapor pressure
#' deficit in kPa.
#' @param model Which model(s) to fit? Defaults to all models. Available
#' options are "BallBerry", "Leuning", "Medlyn_partial", and "Medlyn_full",
#' from Ball et al. (1987), Leuning (1995), and Medlyn et al. (2011).
#' @param D0 Vapor pressure sensitivity of stomata (Leuning 1995)
#' @param ... Arguments to pass on to the nlsLM() function for the Medlyn
#' models.
#'
#' @references
#'
#' Ball JT, Woodrow IE, Berry JA. 1987. A model predicting stomatal
#' conductance and its contribution to the control of photosynthesis
#' under different environmental conditions, in Progress in
#' Photosynthesis Research, Proceedings of the VII International
#' Congress on Photosynthesis, vol. 4, edited by I. Biggins, pp.
#' 221–224, Martinus Nijhoff, Dordrecht, Netherlands.
#'
#' Leuning R. 1995. A critical appraisal of a coupled stomatal-
#' photosynthesis model for C3 plants. Plant Cell Environ 18:339-357
#'
#' Medlyn BE, Duursma RA, Eamus D, Ellsworth DS, Prentice IC, Barton
#' CVM, Crous KY, Angelis PD, Freeman M, Wingate L. 2011. Reconciling
#' the optimal and empirical approaches to modeling stomatal
#' conductance. Glob Chang Biol 17:2134-2144
#'
#' @importFrom minpack.lm nlsLM
#'
#' @return fit_gs_model fits one or more stomatal conductance models to the
#' data. The top level of the output list is named after the fitted model,
#' while the second level contains the Model, Parameters, and Graph, in that
#' order.
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
#' # Convert RH to a proportion
#' data$RH <- data$RHcham / 100
#'
#' # Fit stomatal conductance models
#' # Can specify a single model, or all as below
#' fits <- fit_gs_model(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     C_air = "Ca",
#'     g_sw = "gsw",
#'     RH = "RH",
#'     VPD = "VPDleaf"
#'   ),
#'   model = c(
#'     "BallBerry",
#'     "Leuning",
#'     "Medlyn_partial",
#'     "Medlyn_full"
#'   ),
#'   D0 = 3
#' )
#'
#' # Look at BallBerry model summary:
#' summary(fits[["BallBerry"]][["Model"]])
#'
#' # Look at BallBerry parameters
#' fits[["BallBerry"]][["Parameters"]]
#'
#' # Look at BallBerry plot
#' fits[["BallBerry"]][["Graph"]]
#'
#' # Fit many g_sw models
#' # Set your grouping variable
#' # Here we are grouping by Qin and individual
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#'
#' fits <- fit_many(data,
#'   varnames = list(
#'     A_net = "A",
#'     C_air = "Ca",
#'     g_sw = "gsw",
#'     RH = "RH",
#'     VPD = "VPDleaf"
#'   ),
#'   funct = fit_gs_model,
#'   group = "Q_2"
#' )
#'
#' # Look at the Medlyn_partial outputs at 750 PAR
#' # Model summary
#' summary(fits[["750"]][["Medlyn_partial"]][["Model"]])
#'
#' # Model parameters
#' fits[["750"]][["Medlyn_partial"]][["Parameters"]]
#'
#' # Graph
#' fits[["750"]][["Medlyn_partial"]][["Graph"]]
#'
#' # Compile parameter outputs for BallBerry model
#' # Note that it's the first element for each PAR value
#' # First compile list of BallBerry fits
#' bbmods <- compile_data(
#'   data = fits,
#'   output_type = "list",
#'   list_element = 1
#' )
#' # Now compile the parameters (2nd element) into a dataframe
#' bbpars <- compile_data(
#'   data = bbmods,
#'   output_type = "dataframe",
#'   list_element = 2
#' )
#'
#' # Convert group variable back to numeric
#' bbpars$ID <- as.numeric(bbpars$ID)
#'
#' # Take quick look at light response of intercept parameters
#' plot(g0 ~ ID, bbpars)
#'
#' # Compile graphs
#' graphs <- compile_data(
#'   data = bbmods,
#'   output_type = "list",
#'   list_element = 3
#' )
#'
#' # Look at 3rd graph
#' graphs[[3]]
#' }
fit_gs_model <- function(data, varnames = list(
                           A_net = "A_net",
                           C_air = "C_air",
                           g_sw = "g_sw",
                           RH = "RH",
                           VPD = "VPD"
                         ),
                         model = c(
                           "BallBerry",
                           "Leuning",
                           "Medlyn_partial",
                           "Medlyn_full"
                         ),
                         D0 = 3,
                         ...) {
  # Check: are the models available?
  if (TRUE %in% c(!model %in% c(
    "BallBerry",
    "Leuning",
    "Medlyn_partial",
    "Medlyn_full"
  ))) {
    print("Specified model is not available. Current supported models are
          BallBerry, Leuning, Medlyn_partial, and Medlyn_full.")
    stop()
  }
  # Locally bind variables - avoids notes on check package
  A_net <- NULL
  C_air <- NULL
  g_sw <- NULL
  RH <- NULL
  VPD <- NULL
  # Assign variable names
  data$A_net <- data[, varnames$A_net]
  data$C_air <- data[, varnames$C_air]
  data$g_sw <- data[, varnames$g_sw]
  data$RH <- data[, varnames$RH]
  data$VPD <- data[, varnames$VPD]
  # Create empty list for number of output elements
  models <- vector("list", length(model))
  names(models) <- model
  # BallBerry Model
  if ("BallBerry" %in% model) {
    models[["BallBerry"]] <- vector("list", 3)
    # Assign linear regression model to element 1
    models[["BallBerry"]][[1]] <-
      lm(
        data = data,
        g_sw ~ gs_mod_ballberry(
          A_net = A_net,
          C_air = C_air,
          RH = RH
        )
      )
    # Extract coefficients
    g0 <- coef(models[["BallBerry"]][[1]])[[1]]
    g1 <- coef(models[["BallBerry"]][[1]])[[2]]
    # Create list element of coefficients
    models[["BallBerry"]][[2]] <- as.data.frame(cbind(g0, g1))
    # Assign graph to element 3
    models[["BallBerry"]][[3]] <- ggplot(
      data,
      aes(x = I(A_net * C_air * RH), y = g_sw)
    ) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
    # Assign names to list
    names(models[["BallBerry"]]) <- c("Model", "Parameters", "Graph")
  }
  if ("Leuning" %in% model) {
    models[["Leuning"]] <- vector("list", 3)
    # Assign regression model to element 1
    models[["Leuning"]][[1]] <- lm(
      data = data,
      g_sw ~ gs_mod_leuning(
        A_net = A_net,
        C_air = C_air,
        D0 = D0,
        VPD = VPD
      )
    )
    # Extract coefficients
    g0 <- coef(models[["Leuning"]][[1]])[[1]]
    g1 <- coef(models[["Leuning"]][[1]])[[2]]
    # Assign coefficients to element 2
    models[["Leuning"]][[2]] <- as.data.frame(cbind(g0, g1))
    # Assign graph to element 3
    models[["Leuning"]][[3]] <- ggplot(
      data,
      aes(
        x = I(A_net /
          (C_air * (1 + VPD * D0))),
        y = g_sw
      )
    ) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
    # Assign names to list elements
    names(models[["Leuning"]]) <- c("Model", "Parameters", "Graph")
  }
  if ("Medlyn_partial" %in% model) {
    models[["Medlyn_partial"]] <- vector("list", 3)
    # Fit model, assign to element 1
    try(models[["Medlyn_partial"]][[1]] <- nlsLM(
      data = data,
      g_sw ~ gs_mod_opti(
        A_net = A_net,
        C_air = C_air,
        VPD = VPD,
        g0,
        g1
      ),
      start = list(
        g0 = 0,
        g1 = 1
      ),
      control = nls.control(maxiter = 1000),
      ...
    ))
    # Extract coefficients and make dataframe
    g0 <- coef(models[["Medlyn_partial"]][[1]])[[1]]
    g1 <- coef(models[["Medlyn_partial"]][[1]])[[2]]

    # Assign coefficients to element 2
    if (is.null(models[["Medlyn_partial"]][[1]]) == TRUE) {
      models[["Medlyn_partial"]][[2]] <- data.frame(cbind("NA", "NA"))
      colnames(models[["Medlyn_partial"]][[2]]) <- c("g0", "g1")
    } else {
      models[["Medlyn_partial"]][[2]] <- as.data.frame(cbind(g0, g1))
    }
    # Create graph, assign to element 3
    models[["Medlyn_partial"]][[3]] <- ggplot(
      data,
      aes(x = I(1.6 / (sqrt(VPD)) *
        (A_net / C_air) + 1.6 *
          (A_net / C_air)), y = g_sw)
    ) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
    # Assign names to list elements
    names(models[["Medlyn_partial"]]) <- c("Model", "Parameters", "Graph")
  }
  if ("Medlyn_full" %in% model) {
    models[["Medlyn_full"]] <- vector("list", 3)
    # Fit model, assign to element 1
    try(models[["Medlyn_full"]][[1]] <- nlsLM(
      data = data,
      g_sw ~ gs_mod_optifull(
        A_net = A_net,
        C_air = C_air,
        VPD = VPD,
        g0,
        g1,
        gk
      ),
      start = list(
        g0 = 0,
        g1 = 1,
        gk = 1
      ),
      control = nls.control(maxiter = 1000),
      ...
    ))
    # Extract coefficients and make dataframe
    g0 <- coef(models[["Medlyn_full"]][[1]])[[1]]
    g1 <- coef(models[["Medlyn_full"]][[1]])[[2]]
    gk <- coef(models[["Medlyn_full"]][[1]])[[3]]
    # Assign coefficients to element 2
    if (is.null(models[["Medlyn_full"]][[1]]) == TRUE) {
      models[["Medlyn_full"]][[2]] <- data.frame(cbind("NA", "NA", "NA"))
      colnames(models[["Medlyn_full"]][[2]]) <- c("g0", "g1", "gk")
    } else {
      models[["Medlyn_full"]][[2]] <- as.data.frame(cbind(g0, g1, gk))
    }
    # Create graph, assign to element 3
    models[["Medlyn_full"]][[3]] <- ggplot(
      data,
      aes(x = I(1.6 / ((VPD)^(1 - gk)) *
        (A_net / C_air) + 1.6 *
          (A_net / C_air)), y = g_sw)
    ) +
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_point() +
      theme_bw()
    # Assign names to list elements
    names(models[["Medlyn_full"]]) <- c("Model", "Parameters", "Graph")
  }
  # Add new models here, and make sure their names are available in the model
  # argument at the top.
  # Return list of model outputs
  return(models)
}
