#' Fitting temperature responses
#'
#' @param data Dataframe with temperature response variables
#' @param varnames Variable names, where Par is the parameter of interest, and
#' T_leaf is the leaf temperature in K.
#' @param model Which temperature response model do you want to use? Defaults
#' to all: Arrhenius, Heskel, Kruse, Medlyn, MMRT, Quadratic, and Topt.
#' @param start List of starting parameters for the nls model fits. a, b, and c
#' are needed for the Heskel model, dEa, Ea_ref, and Par_ref are needed for the
#' Kruse model, Ea, Par25, and Hd are all needed for the Medlyn and Topt models
#' while the Medlyn model also requires dS, and dCP, dG, and dH are all for the
#' MMRT model.
#' @param setvar Which variable to set as constant for the Medlyn model?
#' Defaults to "none", while "Hd" and "dS" options are available.
#' @param hdset Which value should Hd be set to when setvar = "Hd"? Specify
#' in J/mol.
#' @param dSset Which value should dS be set to when setvar = "dS"? Specify
#' in J/mol/K.
#' @param title Title of output graphs
#' @param ... Further arguments to pass on to the nlsLM() function
#'
#' @return fit_t_response fits one or more temperature response models to a
#' dataset, returning a list of lists. The parent list contains the models,
#' while the child list for each model contains the fitted model in element
#' 1, the coefficients in element 2, and a graph in element 3.
#' @references
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#'
#' Heskel MA, O'Sullivan OS, Reich PB, Tjoelker MG, Weerasinghe LK,
#' Penillard A, Egerton JJG, Creek D, Bloomfield KJ, Xiang J, Sinca F,
#' Stangl ZR, la Torre AM, Griffin KL, Huntingford C, Hurry V, Meir P,
#' Turnbull MH, Atkin OK. 2016. Convergence in the temperature response
#' of leaf respiration across biomes and plant functional types. PNAS
#' 113:3832-3837
#'
#' Hobbs JK, Jiao W, Easter AD, Parker EJ, Schipper LA, Arcus VL.
#' 2013. Change in heat capacity for enzyme catalysis determines
#' temperature dependence of enzyme catalyzed rates. ACS Chemical
#' Biology 8:2388-2393.
#'
#' Kruse J, Adams MA. 2008. Three parameters comprehensively describe
#' the temperature response of respiratory oxygen reduction. Plant
#' Cell Environ 31:954-967
#'
#' Liang LL, Arcus VL, Heskel MA, O'Sullivan OS, Weerasinghe LK,
#' Creek D, Egerton JJG, Tjoelker MG, Atkin OK, Schipper LA. 2018.
#' Macromolecular rate theory (MMRT) provides a thermodynamics
#' rationale to underpin the convergent temperature response in
#' plant leaf respiration. Glob Chang Biol 24:1538-1547
#'
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#'
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
#' @examples
#' \donttest{
#' # Read in data
#' data <- read.csv(system.file("extdata", "A_Ci_T_data.csv",
#'   package = "photosynthesis"
#' ),
#' stringsAsFactors = FALSE
#' )
#'
#' library(tidyr)
#'
#' # Round temperatures to group them appropriately
#' # Use sequential rounding
#' data$T2 <- round(data$Tleaf, 1)
#' data$T2 <- round(data$Tleaf, 0)
#'
#' # Look at unique values to detect rounding issues
#' unique(data$T2)
#'
#' # Some still did not round correctly,
#' # manually correct
#' for (i in 1:nrow(data)) {
#'   if (data$T2[i] == 18) {
#'     data$T2[i] <- 17
#'   }
#'   if (data$T2[i] == 23) {
#'     data$T2[i] <- 22
#'   }
#'   if (data$T2[i] == 28) {
#'     data$T2[i] <- 27
#'   }
#'   if (data$T2[i] == 33) {
#'     data$T2[i] <- 32
#'   }
#'   if (data$T2[i] == 38) {
#'     data$T2[i] <- 37
#'   }
#' }
#'
#' # Make sure it is a character string for grouping
#' data$T2 <- as.character(data$T2)
#'
#' # Create grouping variable by ID and measurement temperature
#' data <- unite(data,
#'   col = "ID2", c("ID", "T2"),
#'   sep = "_"
#' )
#'
#' # Split by temperature group
#' data <- split(data, data$ID2)
#'
#' # Obtain mean temperature for group so temperature
#' # response fitting is acceptable later, round to
#' # 2 decimal places
#' for (i in 1:length(data)) {
#'   data[[i]]$Curve_Tleaf <- round(mean(data[[i]]$Tleaf), 2)
#' }
#'
#' # Convert from list back to dataframe
#' data <- do.call("rbind", data)
#'
#' # Parse grouping variable by ID and measurement temperature
#' data <- separate(data,
#'   col = "ID2", into = c("ID", "T2"),
#'   sep = "_"
#' )
#'
#' # Make sure number of values matches number of measurement
#' # temperatures. May vary slightly if plants had slightly
#' # different leaf temperatures during the measurements
#' unique(data$Curve_Tleaf)
#'
#' # Create ID column to curve fit by ID and temperature
#' data <- unite(data,
#'   col = "ID2", c("ID", "Curve_Tleaf"),
#'   sep = "_"
#' )
#'
#' # Convert leaf temperature to K
#' data$T_leaf <- data$Tleaf + 273.15
#'
#' # Fit many CO2 response curves
#' fits2 <- fit_many(
#'   data = data,
#'   group = "ID2",
#'   varnames = list(
#'     A_net = "A",
#'     C_i = "Ci",
#'     T_leaf = "T_leaf",
#'     PPFD = "Qin",
#'     g_mc = "g_mc"
#'   ),
#'   funct = fit_aci_response,
#'   alphag = 0
#' )
#'
#' # Extract ACi parameters
#' pars <- compile_data(fits2,
#'   output_type = "dataframe",
#'   list_element = 1
#' )
#'
#' # Extract ACi graphs
#' graphs <- compile_data(fits2,
#'   output_type = "list",
#'   list_element = 2
#' )
#'
#' # Parse the ID variable
#' pars <- separate(pars, col = "ID", into = c("ID", "Curve_Tleaf"), sep = "_")
#'
#' # Make sure curve leaf temperature is numeric
#' pars$Curve_Tleaf <- as.numeric(pars$Curve_Tleaf)
#' pars$T_leaf <- pars$Curve_Tleaf + 273.15
#'
#' # Fit all models, set Hd to constant in Medlyn model
#' out <- fit_t_response(
#'   data = pars[pars$ID == "S2", ],
#'   varnames = list(
#'     Par = "V_cmax",
#'     T_leaf = "T_leaf"
#'   ),
#'   setvar = "Hd",
#'   hdset = 200000
#' )
#'
#' out[["Arrhenius"]][["Graph"]]
#' out[["Heskel"]][["Graph"]]
#' out[["Kruse"]][["Graph"]]
#' out[["Medlyn"]][["Graph"]]
#' out[["MMRT"]][["Graph"]]
#' out[["Quadratic"]][["Graph"]]
#' out[["Topt"]][["Graph"]]
#' }
fit_t_response <- function(data,
                           varnames = list(
                             Par = "Par",
                             T_leaf = "T_leaf"
                           ),
                           model = c(
                             "Arrhenius",
                             "Kruse",
                             "Heskel",
                             "Medlyn",
                             "MMRT",
                             "Quadratic",
                             "Topt"
                           ),
                           start = list(
                             a = 1,
                             b = 1,
                             c = 1,

                             dEa = 1,
                             Ea_ref = 1,
                             Par_ref = 1,

                             Ea = 40000,
                             Par25 = 50,
                             Hd = 200000,
                             dS = 650,

                             dCp = 1,
                             dG = 1,
                             dH = 1
                           ),
                           setvar = "none",
                           hdset = 200000,
                           dSset = 650,
                           title = NULL,
                           ...) {
  # Check: are the models available?
  if (TRUE %in% c(!model %in% c(
    "Arrhenius",
    "Kruse",
    "Heskel",
    "Medlyn",
    "MMRT",
    "Quadratic",
    "Topt"
  ))) {
    print("Specified model is not available. Current supported models are
          Arrhenius, Kruse, Heskel, Medlyn, MMRT, Quadratic, and Topt.")
    stop()
  }
  # Locally bind variables - avoids notes on check package
  Par <- NULL
  T_leaf <- NULL
  # Set variable names
  data$Par <- data[, varnames$Par]
  data$T_leaf <- data[, varnames$T_leaf]
  # Create empty list for number of output elements
  models <- vector("list", length(model))
  names(models) <- model
  # Arrhenius model
  if ("Arrhenius" %in% model) {
    models[["Arrhenius"]] <- vector("list", 3)
    try(models[["Arrhenius"]][[1]] <- nlsLM(
      data = data,
      Par ~ Par25 *
        t_response_arrhenius(Ea,
          T_leaf = T_leaf
        ),
      start = start[c("Ea", "Par25")],
      lower = c(0, 0),
      upper = c(1e10, 10 * max(data$Par)),
      control = nls.control(maxiter = 100),
      ...
    ))
    # Add parameter outputs to output list
    # Assign coefficients to element 2
    if (is.null(models[["Arrhenius"]][[1]]) == TRUE) {
      models[["Arrhenius"]][[2]] <- data.frame(cbind("NA", "NA"))
      colnames(models[["Arrhenius"]][[2]]) <- c("Ea", "Par25")
    } else {
      models[["Arrhenius"]][[2]] <- data.frame(rbind(coef(models[["Arrhenius"]][[1]])))
    }
    # Add graph to output list
    models[["Arrhenius"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ I(models[["Arrhenius"]][[2]]$Par25[1] *
          (t_response_arrhenius(
            T_leaf = x,
            Ea = models[["Arrhenius"]][[2]]$Ea[1]
          ))),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["Arrhenius"]]) <- c("Model", "Parameters", "Graph")
  }
  # Heskel model
  if ("Heskel" %in% model) {
    models[["Heskel"]] <- vector("list", 3)
    try(models[["Heskel"]][[1]] <- nlsLM(
      data = data,
      log(Par) ~
      t_response_heskel(a,
        b,
        c,
        T_leaf = T_leaf
      ),
      start = start[c("a", "b", "c")],
      control =
        nls.control(maxiter = 100),
      ...
    ))
    # Add parameter outputs to output list
    # Assign coefficients to element 2
    if (is.null(models[["Heskel"]][[1]]) == TRUE) {
      models[["Heskel"]][[2]] <- data.frame(cbind("NA", "NA", "NA"))
      colnames(models[["Heskel"]][[2]]) <- c("a", "b", "c")
    } else {
      models[["Heskel"]][[2]] <- data.frame(rbind(coef(models[["Heskel"]][[1]])))
    }
    # Add graph to output list
    models[["Heskel"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ exp(t_response_heskel(
          T_leaf = x,
          a = models[["Heskel"]][[2]]$a[1],
          b = models[["Heskel"]][[2]]$b[1],
          c = models[["Heskel"]][[2]]$c[1]
        )),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["Heskel"]]) <- c("Model", "Parameters", "Graph")
  }
  # Kruse model
  if ("Kruse" %in% model) {
    models[["Kruse"]] <- vector("list", 3)
    data$T2 <- ((data$T_leaf) - 298.15) /
      ((data$T_leaf) * 298.15)
    try(models[["Kruse"]][[1]] <- nlsLM(
      data = data,
      log(Par) ~
      t_response_arrhenius_kruse(dEa,
        Ea_ref,
        Par_ref,
        T2 = T2
      ),
      start = start[c(
        "dEa",
        "Ea_ref",
        "Par_ref"
      )],
      control = nls.control(maxiter = 100),
      ...
    ))
    # Add parameter outputs to output list
    # Assign coefficients to element 2
    if (is.null(models[["Kruse"]][[1]]) == TRUE) {
      models[["Kruse"]][[2]] <- data.frame(cbind("NA", "NA", "NA"))
      colnames(models[["Kruse"]][[2]]) <- c("dEa", "Ea_ref", "Par_ref")
    } else {
      models[["Kruse"]][[2]] <- data.frame(rbind(coef(models[["Kruse"]][[1]])))
    }
    # Add graph to output list
    models[["Kruse"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~
        exp(t_response_arrhenius_kruse(
          T2 = ((x) - 298.15) /
            ((x) * 298.15),
          dEa = models[["Kruse"]][[2]]$dEa[1],
          Ea_ref = models[["Kruse"]][[2]]$Ea_ref[1],
          Par_ref = models[["Kruse"]][[2]]$Par_ref[1]
        )),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["Kruse"]]) <- c("Model", "Parameters", "Graph")
  }
  # Medlyn model
  if ("Medlyn" %in% model) {
    models[["Medlyn"]] <- vector("list", 3)
    # Fit both Hd and dS
    if (setvar == "none") {
      # Basically, use Arrhenius curve to feed Ea into Medlyn function start
      # Try approach where you start Hd from 1 to 1000
      # select minimum residual
      modeltest <- nlsLM(
        data = data,
        Par ~ Par25 * t_response_arrhenius(Ea,
          T_leaf = T_leaf
        ),
        start = start[c("Ea", "Par25")],
        lower = c(0, 0),
        upper = c(1e10, 10 * max(data$Par)),
        control = nls.control(maxiter = 100)
      )
      # Create empty dataframe to fill with 1000 curve fits
      model_fm <- as.data.frame(cbind(
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(varnames$Par[[1]], 1000)
      ))
      # Assign column names
      colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
      # Make sure variabel classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      model_full <- vector("list", 1000)
      # Run through 1000 instances of the model
      # TryCatch is used to deal with failed fits
      for (i in 1:1000) {
        # Fit model
        model_full[[i]] <- tryCatch(nlsLM(
          data = data,
          Par ~ Par25 *
            t_response_arrhenius_medlyn(Ea,
              Hd,
              dS,
              T_leaf =
                T_leaf
            ),
          start = list(
            Ea = coef(modeltest)[["Ea"]],
            Hd = i * 1000,
            dS = dSset,
            Par25 = coef(modeltest)[["Par25"]]
          ),
          lower = c(0, 0, 0, 0),
          upper = c(
            1000000, 2000000, 20000,
            1.5 * max(data$Par)
          ),
          control = nls.control(maxiter = 100)
        ),
        error = function(e) paste(NA)
        )
        # Extract coefficients
        model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[["Ea"]],
          error = function(e) paste(NA)
        )
        model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[["Hd"]],
          error = function(e) paste(NA)
        )
        model_fm$dS[i] <- tryCatch(coef(model_full[[i]])[["dS"]],
          error = function(e) paste(NA)
        )
        model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[["Par25"]],
          error = function(e) paste(NA)
        )
        model_fm$residual[i] <- tryCatch(sum((model_full[[i]]$m$resid())^2),
          error = function(e) paste(NA)
        )
      }
      # Ensure variable classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      # Select best model with minimum residuals
      model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
      model_fm <- model_fm[model_fm$Ea != 1000000, ]
      model_fm <- model_fm[model_fm$Hd != 2000000, ]
      model_fm <- model_fm[model_fm$dS != 20000, ]
      model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
      model_full <- model_full[[as.numeric(rownames(model_fm))]]
      # Add model to output list
      models[["Medlyn"]][[1]] <- model_full
      # Add parameter outputs to output list
      models[["Medlyn"]][[2]] <- data.frame(rbind(coef(model_full)))
      models[["Medlyn"]][[2]]$T_leaf <- mean(data$T_leaf)
      # Add graph to output list
      models[["Medlyn"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
        # Add axis labels
        labs(
          x = expression("T_leaf (K)"),
          y = varnames$Par
        ) +
        # Add title
        ggtitle(label = title) +
        # Add fitted smoothing function
        geom_smooth(
          method = "lm",
          formula = y ~ I(models[["Medlyn"]][[2]]$Par25[1] *
            t_response_arrhenius_medlyn(
              T_leaf = x,
              Ea = models[["Medlyn"]][[2]]$Ea[1],
              Hd = models[["Medlyn"]][[2]]$Hd[1],
              dS = models[["Medlyn"]][[2]]$dS[1]
            )),
          size = 2
        ) +
        # Add points
        geom_point(size = 2) +
        # Use clean theme
        theme_bw()
      # Name outputs
      names(models[["Medlyn"]]) <- c("Model", "Parameters", "Graph")
    }
    # Just fit dS
    if (setvar == "Hd") {
      # Basically, use Arrhenius curve to feed Ea into Medlyn function start
      # Try approach where you start Hd from 1 to 1000
      # select minimum residual
      modeltest <- nlsLM(
        data = data,
        Par ~ Par25 * t_response_arrhenius(Ea,
          T_leaf = T_leaf
        ),
        start = start[c("Ea", "Par25")],
        lower = c(0, 0),
        upper = c(1e10, 10 * max(data$Par)),
        control = nls.control(maxiter = 100)
      )
      # Create empty dataframe to fill with 1000 curve fits
      model_fm <- as.data.frame(cbind(
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(varnames$Par[[1]], 1000)
      ))
      # Assign column names
      colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
      # Make sure variabel classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      model_full <- vector("list", 1000)
      # Run through 1000 instances of the model
      # TryCatch is used to deal with failed fits
      for (i in 1:1000) {
        # Fit model
        model_full[[i]] <- tryCatch(nlsLM(
          data = data,
          Par ~ Par25 *
            t_response_arrhenius_medlyn(Ea,
              Hd = hdset,
              dS,
              T_leaf =
                T_leaf
            ),
          start = list(
            Ea = coef(modeltest)[["Ea"]],
            dS = i,
            Par25 = coef(modeltest)[["Par25"]]
          ),
          lower = c(0, 0, 0),
          upper = c(
            1000000, 20000,
            1.5 * max(data$Par)
          ),
          control = nls.control(maxiter = 100)
        ),
        error = function(e) paste(NA)
        )
        # Extract coefficients
        model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[["Ea"]],
          error = function(e) paste(NA)
        )
        model_fm$Hd[i] <- hdset
        model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[["Par25"]],
          error = function(e) paste(NA)
        )
        model_fm$dS[i] <- tryCatch(coef(model_full[[i]])[["dS"]],
          error = function(e) paste(NA)
        )
        model_fm$residual[i] <- tryCatch(sum((model_full[[i]]$m$resid())^2),
          error = function(e) paste(NA)
        )
      }
      # Ensure variable classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      # Select best model with minimum residuals
      model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
      model_fm <- model_fm[model_fm$Ea != 1000000, ]
      model_fm <- model_fm[model_fm$dS != 20000, ]
      model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
      model_full <- model_full[[as.numeric(rownames(model_fm))]]
      # Add model to output list
      models[["Medlyn"]][[1]] <- model_full
      # Add parameter outputs to output list
      models[["Medlyn"]][[2]] <- data.frame(rbind(coef(model_full)))
      models[["Medlyn"]][[2]]$Hd <- hdset
      models[["Medlyn"]][[2]]$T_leaf <- mean(data$T_leaf)
      # Add graph to output list
      models[["Medlyn"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
        # Add axis labels
        labs(
          x = expression("T_leaf (K)"),
          y = varnames$Par
        ) +
        # Add title
        ggtitle(label = title) +
        # Add fitted smoothing function
        geom_smooth(
          method = "lm",
          formula = y ~ I(models[["Medlyn"]][[2]]$Par25[1] *
            t_response_arrhenius_medlyn(
              T_leaf = x,
              Ea = models[["Medlyn"]][[2]]$Ea[1],
              Hd = models[["Medlyn"]][[2]]$Hd[1],
              dS = models[["Medlyn"]][[2]]$dS[1]
            )),
          size = 2
        ) +
        # Add points
        geom_point(size = 2) +
        # Use clean theme
        theme_bw()
      # Name outputs
      names(models[["Medlyn"]]) <- c("Model", "Parameters", "Graph")
    }
    # Just fit Hd
    if (setvar == "dS") {
      # Basically, use Arrhenius curve to feed Ea into Medlyn function start
      # Try approach where you start Hd from 1 to 1000
      # select minimum residual
      modeltest <- nlsLM(
        data = data,
        Par ~ Par25 * t_response_arrhenius(Ea,
          T_leaf = T_leaf
        ),
        start = start[c("Ea", "Par25")],
        lower = c(0, 0),
        upper = c(1e10, 10 * max(data$Par)),
        control = nls.control(maxiter = 100)
      )
      # Create empty dataframe to fill with 1000 curve fits
      model_fm <- as.data.frame(cbind(
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(0, 1000),
        rep(varnames$Par[[1]], 1000)
      ))
      # Assign column names
      colnames(model_fm) <- c("Ea", "Hd", "Par25", "dS", "residual", "Parameter")
      # Make sure variabel classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      model_full <- vector("list", 1000)
      # Run through 1000 instances of the model
      # TryCatch is used to deal with failed fits
      for (i in 1:1000) {
        # Fit model
        model_full[[i]] <- tryCatch(nlsLM(
          data = data,
          Par ~ Par25 *
            t_response_arrhenius_medlyn(Ea,
              Hd,
              dS = dSset,
              T_leaf =
                T_leaf
            ),
          start = list(
            Ea = coef(modeltest)[["Ea"]],
            Hd = i * 1000,
            Par25 = coef(modeltest)[["Par25"]]
          ),
          lower = c(0, 0, 0),
          upper = c(
            1000000, 2000000,
            1.5 * max(data$Par)
          ),
          control = nls.control(maxiter = 100)
        ),
        error = function(e) paste(NA)
        )
        # Extract coefficients
        model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[["Ea"]],
          error = function(e) paste(NA)
        )
        model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[["Hd"]],
          error = function(e) paste(NA)
        )
        model_fm$Par25[i] <- tryCatch(coef(model_full[[i]])[["Par25"]],
          error = function(e) paste(NA)
        )
        model_fm$dS[i] <- dSset
        model_fm$residual[i] <- tryCatch(sum((model_full[[i]]$m$resid())^2),
          error = function(e) paste(NA)
        )
      }
      # Ensure variable classes are appropriate
      model_fm$Ea <- as.double(model_fm$Ea)
      model_fm$Hd <- as.double(model_fm$Hd)
      model_fm$dS <- as.double(model_fm$dS)
      model_fm$Par25 <- as.double(model_fm$Par25)
      model_fm$residual <- as.double(model_fm$residual)
      # Select best model with minimum residuals
      model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
      model_fm <- model_fm[model_fm$Ea != 1000000, ]
      model_fm <- model_fm[model_fm$Hd != 2000000, ]
      model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
      model_full <- model_full[[as.numeric(rownames(model_fm))]]
      # Add model to output list
      models[["Medlyn"]][[1]] <- model_full
      # Add parameter outputs to output list
      models[["Medlyn"]][[2]] <- data.frame(rbind(coef(model_full)))
      models[["Medlyn"]][[2]]$dS <- dSset
      models[["Medlyn"]][[2]]$T_leaf <- mean(data$T_leaf)
      # Add graph to output list
      models[["Medlyn"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
        # Add axis labels
        labs(
          x = expression("T_leaf (K)"),
          y = varnames$Par
        ) +
        # Add title
        ggtitle(label = title) +
        # Add fitted smoothing function
        geom_smooth(
          method = "lm",
          formula = y ~ I(models[["Medlyn"]][[2]]$Par25[1] *
            t_response_arrhenius_medlyn(
              T_leaf = x,
              Ea = models[["Medlyn"]][[2]]$Ea[1],
              Hd = models[["Medlyn"]][[2]]$Hd[1],
              dS = models[["Medlyn"]][[2]]$dS[1]
            )),
          size = 2
        ) +
        # Add points
        geom_point(size = 2) +
        # Use clean theme
        theme_bw()
      # Name outputs
      names(models[["Medlyn"]]) <- c("Model", "Parameters", "Graph")
    }
  }
  # MMRT model
  if ("MMRT" %in% model) {
    models[["MMRT"]] <- vector("list", 3)
    try(models[["MMRT"]][[1]] <- nlsLM(
      data = data,
      log(Par) ~ t_response_mmrt(dCp,
        dG,
        dH,
        T_leaf = T_leaf
      ),
      start = start[c(
        "dCp",
        "dG",
        "dH"
      )],
      control = nls.control(maxiter = 100),
      ...
    ))
    # Add parameter outputs to output list
    # Assign coefficients to element 2
    if (is.null(models[["MMRT"]][[1]]) == TRUE) {
      models[["MMRT"]][[2]] <- data.frame(cbind("NA", "NA", "NA"))
      colnames(models[["MMRT"]][[2]]) <- c("dCp", "dG", "dH")
    } else {
      models[["MMRT"]][[2]] <- data.frame(rbind(coef(models[["MMRT"]][[1]])))
    }
    # Add graph to output list
    models[["MMRT"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ exp(
          t_response_mmrt(
            T_leaf = x,
            dCp = models[["MMRT"]][[2]]$dCp[1],
            dG = models[["MMRT"]][[2]]$dG[1],
            dH = models[["MMRT"]][[2]]$dH[1]
          )
        ),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["MMRT"]]) <- c("Model", "Parameters", "Graph")
  }
  # Quadratic model
  if ("Quadratic" %in% model) {
    models[["Quadratic"]] <- vector("list", 3)
    try(models[["Quadratic"]][[1]] <- nlsLM(
      data = data,
      Par ~ t_response_heskel(a,
        b,
        c,
        T_leaf = T_leaf
      ),
      start = start[c("a", "b", "c")],
      control = nls.control(maxiter = 100),
      ...
    ))
    # Add parameter outputs to output list
    # Assign coefficients to element 2
    if (is.null(models[["Quadratic"]][[1]]) == TRUE) {
      models[["Quadratic"]][[2]] <- data.frame(cbind("NA", "NA", "NA"))
      colnames(models[["Quadratic"]][[2]]) <- c("a", "b", "c")
    } else {
      models[["Quadratic"]][[2]] <- data.frame(rbind(coef(models[["Quadratic"]][[1]])))
    }
    # Add graph to output list
    models[["Quadratic"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ (t_response_heskel(
          T_leaf = x,
          a = models[["Quadratic"]][[2]]$a[1],
          b = models[["Quadratic"]][[2]]$b[1],
          c = models[["Quadratic"]][[2]]$c[1]
        )),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["Quadratic"]]) <- c("Model", "Parameters", "Graph")
  }
  # Topt model
  if ("Topt" %in% model) {
    models[["Topt"]] <- vector("list", 3)
    modeltest <- nlsLM(
      data = data,
      Par ~ Par25 * t_response_arrhenius(Ea,
        T_leaf = T_leaf
      ),
      start = start[c("Ea", "Par25")],
      lower = c(0, 0),
      upper = c(1e10, 10 * max(data$Par)),
      control = nls.control(maxiter = 100)
    )
    # Create empty dataframe to fill with 1000 curve fits
    model_fm <- as.data.frame(cbind(
      rep(0, 1000),
      rep(0, 1000),
      rep(0, 1000),
      rep(0, 1000),
      rep(0, 1000),
      rep(varnames$Par[[1]], 1000)
    ))
    # Assign column names
    colnames(model_fm) <- c(
      "Ea", "Hd", "kopt", "Topt", "residual",
      "Parameter"
    )
    # Make sure variable classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$kopt <- as.double(model_fm$kopt)
    model_fm$Topt <- as.double(model_fm$Topt)
    model_fm$residual <- as.double(model_fm$residual)
    model_full <- vector("list", 1000)
    # Run through 1000 instances of the model
    # TryCatch is used to deal with failed fits
    for (i in 1:1000) {
      # Fit model
      model_full[[i]] <- tryCatch(nlsLM(
        data = data,
        Par ~ kopt *
          t_response_arrhenius_topt(Ea,
            Hd,
            Topt,
            T_leaf =
              T_leaf
          ),
        start = list(
          Ea = coef(modeltest)[["Ea"]],
          Hd = i * 1000,
          kopt = max(data$Par),
          Topt = max(data$T_leaf)
        ),
        lower = c(0, 0, 0, 0),
        upper = c(
          1000000, 2000000, max(data$Par) + 1,
          max(data$T_leaf) + 1
        ),
        control = nls.control(maxiter = 100)
      ),
      error = function(e) paste(NA)
      )
      # Extract coefficients
      model_fm$Ea[i] <- tryCatch(coef(model_full[[i]])[["Ea"]],
        error = function(e) paste(NA)
      )
      model_fm$Hd[i] <- tryCatch(coef(model_full[[i]])[["Hd"]],
        error = function(e) paste(NA)
      )
      model_fm$kopt[i] <- tryCatch(coef(model_full[[i]])[["kopt"]],
        error = function(e) paste(NA)
      )
      model_fm$Topt[i] <- tryCatch(coef(model_full[[i]])[["Topt"]],
        error = function(e) paste(NA)
      )
      model_fm$residual[i] <- tryCatch(sum((model_full[[i]]$m$resid())^2),
        error = function(e) paste(NA)
      )
    }
    # Ensure variable classes are appropriate
    model_fm$Ea <- as.double(model_fm$Ea)
    model_fm$Hd <- as.double(model_fm$Hd)
    model_fm$kopt <- as.double(model_fm$kopt)
    model_fm$Topt <- as.double(model_fm$Topt)
    model_fm$residual <- as.double(model_fm$residual)
    # Select best model with minimum residuals
    model_fm <- model_fm[is.na(model_fm$residual) == FALSE, ]
    model_fm <- model_fm[model_fm$Ea != 1000000, ]
    model_fm <- model_fm[model_fm$Hd != 2000000, ]
    model_fm <- model_fm[model_fm$residual == min(model_fm$residual), ]
    model_full <- model_full[[as.numeric(rownames(model_fm))]]
    # Add model to output list
    models[["Topt"]][[1]] <- model_full
    # Add parameter outputs to output list
    models[["Topt"]][[2]] <- data.frame(rbind(coef(model_full)))
    # Add graph to output list
    models[["Topt"]][[3]] <- ggplot(data, aes(x = T_leaf, y = Par)) +
      # Add axis labels
      labs(
        x = expression("T_leaf (K)"),
        y = varnames$Par
      ) +
      # Add title
      ggtitle(label = title) +
      # Add fitted smoothing function
      geom_smooth(
        method = "lm",
        formula = y ~ I(models[["Topt"]][[2]]$kopt[1] *
          (t_response_arrhenius_topt(
            T_leaf = x,
            Ea = models[["Topt"]][[2]]$Ea[1],
            Hd = models[["Topt"]][[2]]$Hd[1],
            Topt = models[["Topt"]][[2]]$Topt[1]
          ))),
        size = 2
      ) +
      # Add points
      geom_point(size = 2) +
      # Use clean theme
      theme_bw()
    # Name outputs
    names(models[["Topt"]]) <- c("Model", "Parameters", "Graph")
  }
  return(models)
}
