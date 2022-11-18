#' Fit photosynthetic models with gas-exchange data
#' 
#' @param .data A data frame containing plant ecophysiological data. See \code{\link{required_variables}} for the variables required for each model.
#' @param .photo_fun A character string of **photosynthesis** function to call. One of: \code{`r paste0(get_function_types(), collapse = ', ')`}.
#' @param .model A character string of model name to use. See \code{\link{get_all_models}}. 
#' @param .vars A list to rename variables in .data. See \code{\link{required_variables}} for the accepted variable names.  
#' @param .method A character string of the statistical method to use: 'ls' for least-squares and 'brms' for Bayesian model using \code{\link[brms]{brm}}. Default is 'ls'.
#' @param ... Additional arguments passed to specific models. See specific help pages for each type of photosynthetic model:
#' 
#' * Light-response curves \code{\link{fit_aq_response2}}
#' * Light respiration \code{\link{fit_r_light2}}
#' 
#' @param quiet Flag. Should messages be suppressed? Default is FALSE.
#' @param brm_options A list of options passed to \code{\link[brms]{brm}} if `.method = "brms"`. Default is NULL.
#' 
#' @return A fitted model object
#' 
#' * class 'lm' or 'nls' if `method = 'ls'`
#' * class 'brmsfit' if `method = 'brms'`
#' 
#' @note This function will fit models to data but several methods require post-processing to extract meaningful parameter estimates and confidence intervals. See vignettes for further explanation and examples.
#'
#' * Light-response curves: \code{vignette("light-response", package = "photosynthesis")}
#' * Light respiration: \code{vignette("light-respiration", package = "photosynthesis")}
#' 
#' @md
#' @export
fit_photosynthesis = function(
    .data,
    .photo_fun,
    .model = "default",
    .vars = NULL,
    .method = "ls",
    ...,
    quiet = FALSE,
    brm_options = NULL
) {
  
  checkmate::assert_data_frame(.data)
  .photo_fun = match.arg(.photo_fun, choices = get_function_types())
  .model = match.arg(.model, choices = c("default", get_all_models(.photo_fun)))
  if (.model == "default") .model = get_default_model(.photo_fun)
  .vars = substitute(.vars)
  .method = match.arg(.method, choices = c("ls", "brms"))
  checkmate::assert_flag(quiet)
  checkmate::assert_list(brm_options, null.ok = TRUE)
  
  # Rename variables
  if (!is.null(.vars)) {
    .data = rename_variables(.data, .vars)
  }

  assert_required_variables(
    .data = .data,
    .photo_fun = .photo_fun,
    .model = .model,
    .method = .method,
    quiet = quiet
  )
  
  do.call(
    glue::glue("fit_{.photo_fun}2"),
    args = list(
      .data = .data,
      .model = .model,
      .method = .method,
      ...,
      quiet = quiet,
      brm_options = brm_options
    )
  )
  
}

#' Rename variables in .data based on .vars
#' @inheritParams fit_photosynthesis
#' @noRd
rename_variables = function(.data, .vars) {
  
  # I feel like there needs to be a better way to do this
  deparse(.vars) |>
    stringr::str_replace("^list\\(", ".data = dplyr::rename(.data, ") |>
    str2lang() |>
    eval()

  .data
  
}

#' Assert required variables are present in .data
#' @noRd
assert_required_variables = function(.data, .photo_fun, .model, .method, quiet) {
  
  v = required_variables(.model, quiet = TRUE)
  missing_vars = v[!(v %in% colnames(.data))]
  n_missing_vars = length(missing_vars)
  if (n_missing_vars > 0) {
    if (!quiet) {
      message(paste0(".data is missing required variables: {", paste(missing_vars, collapse = ", "), "}"))
      glue::glue(
        "You may need to revise .vars argument by replacing `var_name1`, etc. with variable name in your_data:
    
    fit_photosynthesis(
      .data = your_data,
      .photo_fun = '{.photo_fun}',
      .model = '{.model}',
      .method = '{.method}',
      .vars = list(
        {args}
      ),
      ...
    )\n\n", 
    args = stringr::str_c(stringr::str_c(v, " = var_name", seq_len(n_missing_vars)), collapse = ",\n    ")
      ) |>
        cat()
    }
    stop(".data is missing variables. fit_photosynthesis() not run.")
  }
}
