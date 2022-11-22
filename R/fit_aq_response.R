#' Fit photosynthetic light-response curves
#' 
#' @description We recommend using [fit_photosynthesis()] with argument `.photo_fun = "aq_response"` rather than calling this function directly.
#' 
#' @inheritParams fit_photosynthesis
#' @param usealpha_Q Flag. Should light intensity be multiplied by `alpha_Q` before fitting? Default is FALSE (i.e. assume that '.Q' is absorbed light).
#' @param alpha_Q Number. Absorbance of incident light. Default value is 0.84. Ignored if `usealpha_Q = FALSE`.
#' 
#' @return 
#' 
#' * If `.method = 'ls'`: an [stats::nls()] object.
#' * If `.method = 'brms'`: a [brms::brmsfit()] object.
#' 
#' @note Rd fitted in this way is essentially the same as the Kok (1956) method, and 
#' represents a respiration value in the light that may not be accurate. 
#' Rd output should thus be interpreted more as a residual parameter to ensure 
#' an accurate fit of the light response parameters. Model originally from 
#' Marshall & Biscoe (1980).
#'
#' @references
#' Marshall B, Biscoe P. 1980. A model for C3 leaves describing the
#' dependence of net photosynthesis on irradiance. J Ex Bot 31:29-39
#'
#' @export
#'
#' @examples
#' \donttest{
#' 
#' library(broom)
#' library(dplyr)
#' library(photosynthesis)
#' 
#' # Read in your data
#' dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
#'   read.csv() |>
#'   # Set grouping variable
#'   mutate(group = round(CO2_s, digits = 0)) |>
#'   # For this example, round sequentially due to CO2_s set points
#'   mutate(group = as.factor(round(group, digits = -1)))
#' 
#' # Fit one light-response curve
#' fit = fit_photosynthesis(
#'   .data = filter(dat, group == 600),
#'   .photo_fun = "aq_response",
#'   .vars = list(.A = A, .Q = Qabs),
#' )
#' 
#' # The 'fit' object inherits class 'nls' and many methods can be used
#' 
#' ## Model summary:
#' summary(fit)
#' 
#' ## Estimated parameters:
#' coef(fit)
#' 
#' ## 95% confidence intervals:
#' confint(fit)
#' 
#' ## Tidy summary table using 'broom::tidy()'
#' tidy(fit, conf.int = TRUE, conf.level = 0.95)
#' 
#' # Fit multiple curves with **photosynthesis** and **purrr**
#' 
#' library(purrr)
#' 
#' fits = dat |>
#'   split(~ group) |>
#'   map(fit_photosynthesis, .photo_fun = "aq_response", .vars = list(.A = A, .Q = Qabs))
#' 
#' }
#' 
#' @md
fit_aq_response2 = function(
    .data,
    .model = "default",
    .method = "ls",
    usealpha_Q = FALSE,
    alpha_Q = 0.84,
    quiet = FALSE,
    brm_options = NULL
  ) {
  
  # Checks
  checkmate::assert_flag(usealpha_Q)
  checkmate::assert_number(alpha_Q, na.ok = !usealpha_Q)
  
  # Set light intensity dependent on whether it is incident or
  # absorbed that you want the variables on
  .data = dplyr::mutate(.data, .Qabs = .Q * ifelse(usealpha_Q, alpha_Q, 1)) |>
    dplyr::select(.A, .Q, .Qabs)

  # Fit AQ response model
  fit = switch(
    .method,
    ls = fit_aq_response2_ls(.data),
    brms = fit_aq_response2_brms(.data, brm_options)
  )
 
  fit 
  
}

#' Fit light response using [minpack.lm::nlsLM()]
#' @noRd
fit_aq_response2_ls = function(.data, ...) {
  
  requireNamespace("minpack.lm") || stop("Package not loaded: minpack.lm")
  
  minpack.lm::nlsLM(
    data = .data, 
    .A ~ marshall_biscoe_1980(Q_abs = .data[[".Qabs"]], k_sat, phi_J, theta_J) - Rd,
    # Attempt to estimate starting parameters
    start = get_init_aq_response(.data),
    # Set lower limits
    lower = c(min(.data[[".A"]]), rep(0, 3)),
    # set upper limits
    upper = c(10 * max(abs(.data[[".A"]])), 0.5, 1, max(.data[[".A"]])),
    # set max iterations for curve fitting
    control = nls.lm.control(maxiter = 100)
  )
  
}

#' Fit light response using [brms::brm()]
#' @noRd
fit_aq_response2_brms = function(.data, brm_options, ...) {
  
  requireNamespace("brms") || stop("Package not loaded: brms")
  
  lifecycle::signal_stage("experimental", what = "fit_aq_response2(.method = 'brms')")
  
  do.call(
    brms::brm,
    args = c(
      brm_options,
      list(
        formula = brms::bf(
          .A ~ ((Asat + inv_logit(logitPhiJ) * .Qabs) -
                 sqrt((Asat + inv_logit(logitPhiJ) * .Qabs) ^ 2 -
                        4 * Asat * inv_logit(logitPhiJ) * .Qabs * inv_logit(logitThetaJ))) /
            (2 * inv_logit(logitThetaJ)) - Rd,
          Asat ~ 1,
          logitPhiJ ~ 1,
          logitThetaJ ~ 1,
          Rd ~ 1,
          nl = TRUE
        ), 
        data = .data,
        prior = c(
          brms::prior(normal(20, 10), nlpar = "Asat", lb = 0),
          brms::prior(normal(-2.5, 0.5), nlpar = "logitPhiJ"),
          brms::prior(normal(2.5, 0.5), nlpar = "logitThetaJ"),
          brms::prior(normal(2, 1), nlpar = "Rd", lb = 0)
        )
      )
    )
  )
  
}

#' Get list of starting values for `fit_aq_response()`
#' @noRd
# Get initial values
get_init_aq_response = function(.data) {
  
  x1 = try({unname(coef(lm(.A ~ .Qabs, dplyr::filter(.data, .Qabs < 300))))}, silent = TRUE)
  if (inherits(x1, "try-error")) x1 = c(0.06, 1)
  
  list(
    k_sat = max(.data[[".A"]]),
    phi_J = x1[2],
    theta_J = 0.85,
    Rd = -x1[1]
  )
  
}

#' Fitting light responses of net CO2 assimilation
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' Please use `fit_aq_response2()`.
#' 
#' @param data Dataframe containing CO2 assimilation light response
#' @param varnames Variable names where varnames = list(A_net = "A_net",
#' PPFD = "PPFD"). A_net is net CO2 assimilation in umol m-2 s-1, PPFD is
#' incident irradiance. PPFD can be corrected for light absorbance by using
#' useapha_Q and setting alpha_Q.
#' @param usealpha_Q Correct light intensity for absorbance? Default is FALSE.
#' @param alpha_Q Absorbance of incident light. Default value is 0.84.
#' @param title Title for graph
#'
#' @return fit_aq_response fits the light response of net CO2 assimilation.
#' Output is a dataframe containing light saturated net CO2 assimilation,
#' quantum yield of CO2 assimilation (phi_J), curvature of the light response
#' (theta_J), respiration (Rd), light compensation point (LCP), and residual
#' sum of squares (resid_SS). Note that Rd fitted in this way is essentially
#' the same as the Kok method, and represents a respiration value in the
#' light that may not be accurate. Rd output should thus be interpreted more
#' as a residual parameter to ensure an accurate fit of the light response
#' parameters. Model originally from Marshall & Biscoe 1980.
#'
#' @references
#' Marshall B, Biscoe P. 1980. A model for C3 leaves describing the
#' dependence of net photosynthesis on irradiance. J Ex Bot 31:29-39
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data = read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit many AQ curves
#' # Set your grouping variable
#' # Here we are grouping by CO2_s and individual
#' data$C_s = (round(data$CO2_s, digits = 0))
#'
#' # For this example we need to round sequentially due to CO2_s setpoints
#' data$C_s = as.factor(round(data$C_s, digits = -1))
#'
#' # To fit one AQ curve
#' fit = fit_aq_response(data[data$C_s == 600, ],
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin"
#'   )
#' )
#'
#' # Print model summary
#' summary(fit[[1]])
#'
#' # Print fitted parameters
#' fit[[2]]
#'
#' # Print graph
#' fit[[3]]
#'
#' # Fit many curves
#' fits = fit_many(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin",
#'     group = "C_s"
#'   ),
#'   funct = fit_aq_response,
#'   group = "C_s"
#' )
#'
#' # Look at model summary for a given fit
#' # First set of double parentheses selects an individual group value
#' # Second set selects an element of the sublist
#' summary(fits[[3]][[1]])
#'
#' # Print the parameters
#' fits[[3]][[2]]
#'
#' # Print the graph
#' fits[[3]][[3]]
#'
#' # Compile graphs into a list for plotting
#' fits_graphs = compile_data(fits,
#'   list_element = 3
#' )
#'
#' # Compile parameters into dataframe for analysis
#' fits_pars = compile_data(fits,
#'   output_type = "dataframe",
#'   list_element = 2
#' )
#' }
#' 
#' @md
fit_aq_response = function(
    data,
    varnames = list(
      A_net = "A_net",
      PPFD = "PPFD"
    ),
    usealpha_Q = FALSE,
    alpha_Q = 0.84,
    title = NULL
) {
  
  lifecycle::deprecate_warn(
    "2.1.1", "fit_aq_response()", 
    "fit_photosynthesis(.photo_fun = 'aq_response')", 
    always = FALSE
  )
  
  # Locally bind variables - avoids notes on check package
  A_net = NULL
  Q_abs = NULL
  # Set variable names
  data$A_net = data[, varnames$A_net]
  # Set light intensity dependent on whether it is incident or
  # absorbed that you want the variables on
  if (usealpha_Q) {
    data$Q_abs = data[, varnames$PPFD] * alpha_Q
  } else {
    data$Q_abs = data[, varnames$PPFD]
  }
  # Create empty list for outputs
  output = list(NULL)
  # Fit AQ response model using nlsLM - this function is more
  # robust (i.e. successful) than regular nls
  output[[1]] = nlsLM(
    data = data, A_net ~ aq_response(k_sat,
      phi_J,
      Q_abs = data$Q_abs,
      theta_J
    ) - Rd,
    # Attempt to estimate starting parameters
    start = list(
      k_sat = max(data$A_net),
      phi_J = coef(lm(
        A_net ~ Q_abs,
        data[data$Q_abs <
          300, ]
      ))[2],
      theta_J = 0.85,
      Rd = -coef(lm(
        A_net ~ Q_abs,
        data[data$Q_abs <
          300, ]
      ))[1]
    ),
    # Set lower limits
    lower = c(
      min(data$A_net),
      0,
      0,
      0
    ),
    # set upper limits
    upper = c(
      10 * max(abs(data$A_net)),
      0.5,
      1,
      max(abs(data$A_net))
    ),
    # set max iterations for curve fitting
    control = nls.lm.control(maxiter = 100)
  )
  # Prepare output dataframe and extract coefficients
  fitted_pars = NULL
  fitted_pars$A_sat = coef(output[[1]])[1]
  fitted_pars$phi_J = coef(output[[1]])[2]
  fitted_pars$theta_J = coef(output[[1]])[3]
  fitted_pars$Rd = coef(output[[1]])[4]
  fitted_pars$LCP = ((coef(output[[1]])[4]) *
    (coef(output[[1]])[4] * coef(output[[1]])[3] -
      coef(output[[1]])[1]) /
    (coef(output[[1]])[2] * (coef(output[[1]])[4] -
      coef(output[[1]])[1])))
  fitted_pars$resid_SSs = sum(resid(output[[1]])^2)
  # Add fitted parameters to output
  output[[2]] = as.data.frame(do.call("cbind", fitted_pars))
  # Create graph
  output[[3]] = ggplot(data, aes(x = Q_abs, y = A_net)) +
    # Add axis labels
    labs(
      x = expression("Irradiance (" * mu * mol ~ m^{
        -2
      } ~ s^
        {
          -1
        } * ")"),
      y = expression(A[net] ~ "(" * mu * mol ~ m^{
        -2
      } ~ s^
        {
          -1
        } * ")")
    ) +
    # Add title
    ggtitle(label = title) +
    # Add fitted smoothing function
    geom_smooth(
      method = "lm", aes(x = Q_abs, y = A_net),
      show.legend = TRUE,
      formula = y ~ I(aq_response(
        k_sat = output[[2]]$A_sat[1],
        phi_J = output[[2]]$phi_J[1],
        Q_abs = x,
        theta_J = output[[2]]$theta_J[1]
      ) -
        output[[2]]$Rd[1]),
      linewidth = 2
    ) +
    # Add points
    geom_point(size = 2) +
    # Use clean theme
    theme_bw()
  # Name outputs
  names(output) = c("Model", "Parameters", "Graph")
  # Return list of outputs
  return(output)
}
