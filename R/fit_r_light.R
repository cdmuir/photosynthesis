#' Fit models to estimate light respiration (\eqn{R_\mathrm{d}})
#' 
#' @description We recommend using \code{\link{fit_photosynthesis}} with argument `.photo_fun = "r_light"` rather than calling this function directly.
#' 
#' @inheritParams fit_photosynthesis
#' @param Q_lower Lower light intensity limit for estimating Rd using `kok_1956` and `yin_etal_2011` models.
#' @param Q_upper Upper light intensity limit for estimating Rd using `kok_1956` and `yin_etal_2011` models
#' @param Q_levels A numeric vector of light intensity levels (\eqn{\mu}mol / mol) for estimating \eqn{R_\mathrm{d}} from the linear region of the A-C curve using the `walker_ort_2015` model.
#' @param C_upper Upper C (\eqn{\mu}mol / mol) limit for estimating \eqn{R_\mathrm{d}} from the linear region of the A-C curve using the `walker_ort_2015` model.
#'
#' @return 
#' 
#' * If `.method = 'ls'`: an \code{\link[stats]{nls}} or \code{\link[stats]{lm}} object.
#' * If `.method = 'brms'`: a \code{\link[brms]{brmsfit}} object.
#' 
#' @note 
#' 
#' Confusingly, \eqn{R_\mathrm{d}} typically denotes respiration in the light, but you might see \eqn{R_\mathrm{day}} or \eqn{R_\mathrm{light}}.
#' 
#' **Models**
#' 
#' *Kok (1956)*
#' 
#' The `kok_1956` model estimates light respiration using the Kok method
#' (Kok, 1956). The Kok method involves looking for a breakpoint in the
#' light response of net CO2 assimilation at very low light intensities
#' and extrapolating from data above the breakpoint to estimate light
#' respiration as the y-intercept. Rd value should be negative,
#' denoting an efflux of CO2.
#'
#' *Yin et al. (2011)*
#' 
#' The `yin_etal_2011` model estimates light respiration according
#' to the Yin *et al.* (2009, 2011) modifications of the Kok
#' method. The modification uses fluorescence data to get a
#' better estimate of light respiration. Rd values should be negative here to 
#' denote an efflux of CO2.
#' 
#' *Walker & Ort (2015)*
#' 
#' The `walker_ort_2015` model estimates light respiration and
#' \eqn{\Gamma*} according to Walker & Ort (2015) using a slope-
#' intercept regression method to find the intercept of multiple
#' A-C curves run at multiple light intensities. The method estimates
#' \eqn{\Gamma*} and \eqn{R_\mathrm{d}}. If estimated  \eqn{R_\mathrm{d}} is 
#' positive this could indicate issues (i.e. leaks) in the gas exchange
#' measurements. \eqn{\Gamma*} is in units of umol / mol and \eqn{R_\mathrm{d}}
#' is in units of \eqn{\mu}mol m\eqn{^{-2}} s\eqn{^{-1}} of respiratory flux. 
#' If using \eqn{C_\mathrm{i}}, the estimated value is technically \eqn{C_\mathrm{i}}*. 
#' You need to use \eqn{C_\mathrm{c}} to get \eqn{\Gamma*} Also note, however, 
#' that the convention in the field is to completely ignore this note.
#'
#'
#' @references
#' Kok B. 1956. On the inhibition of photosynthesis by intense light.
#' Biochimica et Biophysica Acta 21: 234–244
#'
#' Walker BJ, Ort DR. 2015. Improved method for measuring the apparent
#' CO2 photocompensation point resolves the impact of multiple internal
#' conductances to CO2 to net gas exchange. Plant Cell Environ 38:2462-
#' 2474
#'
#' Yin X, Struik PC, Romero P, Harbinson J, Evers JB, van der Putten
#' PEL, Vos J. 2009. Using combined measurements of gas exchange and
#' chlorophyll fluorescence to estimate parameters of a biochemical C3
#' photosynthesis model: a critical appraisal and a new integrated
#' approach applied to leaves in a wheat (Triticum aestivum) canopy.
#' Plant Cell Environ 32:448-464
#'
#' Yin X, Sun Z, Struik PC, Gu J. 2011. Evaluating a new method to
#' estimate the rate of leaf respiration in the light by analysis of
#' combined gas exchange and chlorophyll fluorescence measurements.
#' Journal of Experimental Botany 62: 3489–3499
#'
#' @examples
#' \donttest{
#' 
#' # Walker & Ort (2015) model
#' 
#' library(broom)
#' library(dplyr)
#' library(photosynthesis)
#' 
#' acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
#'   read.csv()
#' 
#' fit = fit_photosynthesis(
#'   .data = acq_data,
#'   .photo_fun = "r_light",
#'   .model = "walker_ort_2015",
#'   .vars = list(.A = A, .Q = Qin, .C = Ci),
#'   C_upper = 300,
#'   # Irradiance levels used in experiment
#'   Q_levels =  c(1500, 750, 375, 125, 100, 75, 50, 25),
#' )
#' 
#' # The 'fit' object inherits class 'lm' and many methods can be used
#' 
#' ## Model summary:
#' summary(fit)
#' 
#' ## Estimated parameters:
#' coef(fit)
#' 
#' ## 95% confidence intervals:
#' ## n.b. these confidence intervals are not correct because the regression is fit 
#' ## sequentially. It ignores the underlying data and uncertainty in estimates of 
#' ## slopes and intercepts with each A-C curve. Use '.method = "brms"' to properly 
#' ## calculate uncertainty. 
#' confint(fit)
#' 
#' ## Tidy summary table using 'broom::tidy()'
#' tidy(fit, conf.int = TRUE, conf.level = 0.95)
#' 
#' ## Calculate residual sum-of-squares
#' sum(resid(fit)^2)
#' 
#' # Yin et al. (2011) model
#' 
#' fit = fit_photosynthesis(
#'   .data = acq_data,
#'   .photo_fun = "r_light",
#'   .model = "yin_etal_2011",
#'   .vars = list(.A = A, .phiPSII = PhiPS2, .Q = Qin),
#'   Q_lower = 20,
#'   Q_upper = 250
#' )
#' 
#' # The 'fit' object inherits class 'lm' and many methods can be used
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
#' ## Calculate residual sum-of-squares
#' sum(resid(fit)^2)
#' 
#' # Kok (1956) model
#' 
#' fit = fit_photosynthesis(
#'   .data = acq_data,
#'   .photo_fun = "r_light",
#'   .model = "kok_1956",
#'   .vars = list(.A = A, .Q = Qin),
#'   Q_lower = 20,
#'   Q_upper = 150
#' )
#' 
#' # The 'fit' object inherits class 'lm' and many methods can be used
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
#' ## Calculate residual sum-of-squares
#' sum(resid(fit)^2)
#' 
#' }
#' @md
#' @rdname fit_r_light2
#' @export
fit_r_light2 = function(
    .data,
    .model = "default",
    .method = "ls",
    Q_lower = NA,
    Q_upper = NA,
    Q_levels = NULL,
    C_upper = NA,
    quiet = FALSE,
    brm_options = NULL
) {
  
  # Checks
  checkmate::assert_number(
    Q_lower, 
    lower = 0,
    na.ok = !(.model %in% c("kok_1956", "yin_etal_2011"))
  )
  checkmate::assert_number(
    Q_upper, 
    lower = Q_lower,
    na.ok = !(.model %in% c("kok_1956", "yin_etal_2011"))
  )
  checkmate::assert_numeric(
    Q_levels, 
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 2L,
    null.ok = !(.model %in% c("default", "walker_ort_2015"))
  )
  checkmate::assert_number(
    C_upper, 
    lower = 0,
    na.ok = !(.model %in% c("default", "walker_ort_2015"))
  )
  
  # Fit model
  fit = switch(
    .method,
    ls = fit_r_light2_ls(.data, .model, Q_lower = Q_lower, Q_upper = Q_upper,
                         Q_levels = Q_levels, C_upper = C_upper),
    brms = fit_r_light2_brms(.data, .model, Q_lower = Q_lower, Q_upper = Q_upper,
                             Q_levels = Q_levels, C_upper = C_upper, 
                             brm_options = brm_options)
  )
  
  fit 
  
}

#' Fit models to estimate light respiration (Rd) using least-squares methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_ls = function(
    .data, 
    .model, 
    Q_lower, 
    Q_upper,
    Q_levels,
    C_upper
) {
  
  do.call(
    glue::glue("fit_r_light2_{.model}_ls"),
    args = list(
      .data = .data, 
      Q_lower = Q_lower, 
      Q_upper = Q_upper,
      Q_levels = Q_levels,
      C_upper = C_upper
    )
  )
  
}

#' Fit models to estimate light respiration (Rd) using Bayesian methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_brms = function(
    .data, 
    .model, 
    Q_lower, 
    Q_upper,
    Q_levels,
    C_upper,
    brm_options
) {

  do.call(
    glue::glue("fit_r_light2_{.model}_brms"),
    args = list(
      .data = .data, 
      Q_lower = Q_lower, 
      Q_upper = Q_upper,
      Q_levels = Q_levels,
      C_upper = C_upper,
      brm_options = brm_options
    )
  )
  
}

#' Fit models to estimate light respiration (Rd) with the Walker & Ort (2015) model using least-squares methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_walker_ort_2015_ls = function(
    .data, 
    Q_levels,
    C_upper,
    ...
) {

  .data = .data |>
    dplyr::filter(.C <= C_upper) |>
    dplyr::mutate(
      # Group by Q_level
      .Q_level = round_to_nearest(.Q, Q_levels)
    )
  
  nlme::lmList(.A ~ .C | .Q_level, data = .data) |>
    coef() |>
    dplyr::mutate(gamma_star = -.C) %>%
    lm(`(Intercept)` ~ gamma_star, data = .)
  
}

#' Fit models to estimate light respiration (Rd) with the Walker & Ort (2015) model using Bayesian methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_walker_ort_2015_brms = function(
    .data, 
    Q_levels,
    C_upper,
    brm_options,
    ...
) {
  
  .data = .data |>
    dplyr::filter(.C <= C_upper) |>
    dplyr::mutate(
      # Group by Q_level
      .Q_level = as.factor(round_to_nearest(.Q, Q_levels))
    )
  
  do.call(
    brms::brm,
    args = c(
      brm_options,
      list(
        formula = .A ~ .C + (1 + .C|.Q_level),
        data = .data
      )
    )
  )
  
}

#' Fit models to estimate light respiration (Rd) with the Yin *et al.* (2011) model using least-squares methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_yin_etal_2011_ls = function(
    .data, 
    Q_lower,
    Q_upper,
    ...
) {
  
  .data |>
    dplyr::filter(.Q >= Q_lower, .Q <= Q_upper) |>
    dplyr::mutate(x_var = .Q * .phiPSII / 4) %>%
    lm(.A ~ x_var, data = .)
  
}

#' Fit models to estimate light respiration (Rd) with the Yin *et al.* (2011) model using Bayesian methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_yin_etal_2011_brms = function(
    .data, 
    Q_lower,
    Q_upper,
    brm_options,
    ...
) {
  
  .data = .data |>
    dplyr::filter(.Q >= Q_lower, .Q <= Q_upper) |>
    dplyr::mutate(x_var = .Q * .phiPSII / 4)
  
  do.call(
    brms::brm,
    args = c(
      brm_options,
      list(
        formula = .A ~ x_var,
        data = .data
      )
    )
  )
  
}

#' Fit models to estimate light respiration (Rd) with the Kok (1956) model using least-squares methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_kok_1956_ls = function(
    .data, 
    Q_lower,
    Q_upper,
    ...
) {
  
  .data |>
    dplyr::filter(.Q >= Q_lower, .Q <= Q_upper) %>%
    lm(.A ~ .Q, data = .)
  
}

#' Fit models to estimate light respiration (Rd) with the Kok (1956) model using Bayesian methods
#' @inheritParams fit_r_light2
#' @noRd
fit_r_light2_kok_1956_brms = function(
    .data, 
    Q_lower,
    Q_upper,
    brm_options,
    ...
) {
  
  .data = .data |>
    dplyr::filter(.Q >= Q_lower, .Q <= Q_upper)
  
  do.call(
    brms::brm,
    args = c(
      brm_options,
      list(
        formula = .A ~ .Q,
        data = .data
      )
    )
  )
  
}

#' Estimating light respiration
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' Please use `fit_r_light2()`.
#' 
#' @param data Dataframe
#' @param varnames List of variable names
#' @param PPFD_lower Lower light intensity limit for estimating Rlight
#' (Kok & Yin)
#' @param PPFD_upper Upper light intensity limit for estimating Rlight
#' (Kok & Yin)
#'
#' @param P Atmospheric pressure in kPa (Walker & Ort, 2015)
#' @param C_i_threshold Threshold C_i (in umol / mol) to cut data to
#' linear region for fitting light respiration and gamma_star
#' (Walker & Ort, 2015)
#'
#' @return fit_r_light_kok estimates light respiration using the Kok method
#' (Kok, 1956). The Kok method involves looking for a breakpoint in the
#' light response of net CO2 assimilation at very low light intensities
#' and extrapolating from data above the breakpoint to estimate light
#' respiration as the y-intercept. r_light value should be negative,
#' denoting an efflux of CO2.
#'
#' fit_r_light_WalkerOrt estimates light respiration and
#' GammaStar according to Walk & Ort (2015) using a slope-
#' intercept regression method to find the intercept of multiple
#' ACi curves run at multiple light intensities. Output GammaStar and
#' respiration should be negative If output respiration is positive
#' this could indicate issues (i.e. leaks) in the gas exchange
#' measurements. GammaStar is output in umol mol-1, and respiration
#' is output in umol m-2 s-1 of respiratory flux. Output is a list
#' containing the slope intercept regression model, a graph of the fit,
#' and estimates of the coefficients. NOTE: if using C_i, the output value
#' is technically C_istar. You need to use Cc to get GammaStar. Also note,
#' however, that the convention in the field is to completely ignore this note.
#'
#' fit_r_light_yin estimates light respiration according
#' to the Yin et al. (2009, 2011) modifications of the Kok
#' method. The modification uses fluorescence data to get a
#' better estimate of light respiration. Note that respiration
#' output should be negative here to denote an efflux of CO2.
#'
#' @references
#' Kok B. 1956. On the inhibition of photosynthesis by intense light.
#' Biochimica et Biophysica Acta 21: 234–244
#'
#' Walker BJ, Ort DR. 2015. Improved method for measuring the apparent
#' CO2 photocompensation point resolves the impact of multiple internal
#' conductances to CO2 to net gas exchange. Plant Cell Environ 38:2462-
#' 2474
#'
#' Yin X, Struik PC, Romero P, Harbinson J, Evers JB, van der Putten
#' PEL, Vos J. 2009. Using combined measurements of gas exchange and
#' chlorophyll fluorescence to estimate parameters of a biochemical C3
#' photosynthesis model: a critical appraisal and a new integrated
#' approach applied to leaves in a wheat (Triticum aestivum) canopy.
#' Plant Cell Environ 32:448-464
#'
#' Yin X, Sun Z, Struik PC, Gu J. 2011. Evaluating a new method to
#' estimate the rate of leaf respiration in the light by analysis of
#' combined gas exchange and chlorophyll fluorescence measurements.
#' Journal of Experimental Botany 62: 3489–3499
#'
#' @importFrom nlme lmList
#' @importFrom stats coef
#' @importFrom stats lm
#'
#' @examples
#' \donttest{
#' # FITTING KOK METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data = read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit light respiration with Kok method
#' r_light = fit_r_light_kok(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin"
#'   ),
#'   PPFD_lower = 20,
#'   PPFD_upper = 150
#' )
#' # Return r_light
#' r_light
#'
#' # FITTING WALKER-ORT METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data = read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit the Walker-Ort method for GammaStar and light respiration
#' walker_ort = fit_r_light_WalkerOrt(data,
#'   varnames = list(
#'     A_net = "A",
#'     C_i = "Ci",
#'     PPFD = "Qin"
#'   )
#' )
#' # Extract model
#' summary(walker_ort[[1]])
#'
#' # View graph
#' walker_ort[[2]]
#'
#' # View coefficients
#' walker_ort[[3]]
#'
#' # FITTING THE YIN METHOD
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data = read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Fit light respiration with Yin method
#' r_light = fit_r_light_yin(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin",
#'     phi_PSII = "PhiPS2"
#'   ),
#'   PPFD_lower = 20,
#'   PPFD_upper = 250
#' )
#' }
#'
#' @rdname fit_r_light
#' @export
fit_r_light_kok = function(
    data,
    varnames = list(
      A_net = "A_net",
      PPFD = "PPFD"
    ),
    PPFD_lower = 40,
    PPFD_upper = 100
) {
  
  lifecycle::deprecate_warn(
    "2.1.1", 
    "fit_r_light_kok()", 
    "fit_r_light2(.model = 'kok_1956')", 
    always = TRUE
  )
  
  # Set variable names
  data$A_net = data[, varnames$A_net]
  data$PPFD = data[, varnames$PPFD]
  # Reduce data to within PPFD range
  data_use = data[data$PPFD < PPFD_upper, ]
  data_use = data_use[data_use$PPFD > PPFD_lower, ]
  # Linear regression to estimate r_light (intercept)
  model = lm(A_net ~ PPFD, data = data_use)
  r_light = coef(model)[1]
  # Output light respiration value
  return(r_light)
}

#' @rdname fit_r_light
#' @export
fit_r_light_WalkerOrt = function(
    data,
    varnames = list(
      A_net = "A_net",
      C_i = "C_i",
      PPFD = "PPFD"
    ),
    P = 100,
    C_i_threshold = 300
) {
  
  lifecycle::deprecate_warn(
    "2.1.1", 
    "fit_r_light_WalkerOrt()", 
    "fit_r_light2(.model = 'walker_ort_2015')", 
    always = TRUE
  )

  # Set variable names
  data$A_net = data[, varnames$A_net]
  data$C_i = data[, varnames$C_i]
  data$PPFD = data[, varnames$PPFD]

  # Locally define slope and intercept for slope-intercept regression
  # This gets rid of a note in the R CMD CHECK
  Slope = NULL
  Intercept = NULL

  # Restrict data analysis by a threshold C_i
  data_use = data[data$C_i < C_i_threshold, ]
  # Convert C_i to units of Pa
  data_use$C_i = data_use$C_i / 1000000 * P * 1000
  # Set PPFD as factor for grouping & round
  # PPFD to nearest 10s
  data_use$PPFD = round(data_use$PPFD, digits = -1)
  data_use$PPFD = as.factor(data_use$PPFD)
  # Construct regressions on the pseudolinear portions of
  # the ACi curves
  model = lmList(A_net ~ C_i | PPFD, data = data_use)
  # Extract coefficients
  coefs = coef(model)
  colnames(coefs) = c("Intercept", "Slope")
  coefs$PPFD = rownames(coefs)
  # Create output list
  output = list(NULL)
  # Run slope-intercept regression model, assign to element 1
  output[[1]] = lm(Intercept ~ Slope,
    data = coefs
  )
  # Create graph, assign to element 2
  output[[2]] = ggplot(coefs, aes(x = Slope, y = Intercept)) +
    labs(x = "Slope", y = "Intercept") +
    geom_smooth(method = "lm", linewidth = 2) +
    geom_point(size = 3) +
    theme_bw()
  # Extract coefficients as per Walker and Ort 2015
  # But convert C_istar to umol mol-1
  GammaStar = -coef(output[[1]])[2] / (P * 1000) * 1000000
  r_light = coef(output[[1]])[1]
  output[[3]] = as.data.frame(cbind(GammaStar, r_light))
  # Return output
  return(output)
}

#' @rdname fit_r_light
#' @export
fit_r_light_yin = function(
    data,
    varnames = list(
      A_net = "A_net",
      PPFD = "PPFD",
      phi_PSII = "phi_PSII"
    ),
    PPFD_lower = 40,
    PPFD_upper = 100
) {
  
  lifecycle::deprecate_warn(
    "2.1.1", 
    "fit_r_light_yin()", 
    "fit_r_light2(.model = 'yin_etal_2011')", 
    always = TRUE
  )
  
  # Set variable names
  data$A_net = data[, varnames$A_net]
  data$phi_PSII = data[, varnames$phi_PSII]
  data$PPFD = data[, varnames$PPFD]
  # Reduce data to within PPFD range
  data_use = data[data$PPFD < PPFD_upper, ]
  data_use = data_use[data_use$PPFD > PPFD_lower, ]
  # Calculate x-variable for Yin method
  data_use$x_var = data_use$PPFD * data_use$phi_PSII / 4
  # Fit linear model for Yin method
  model = lm(A_net ~ x_var, data = data_use)
  r_light = coef(model)[1]
  return(r_light)
}
