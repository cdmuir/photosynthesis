#' Make lists of parameters for \code{photosynthesis}
#'
#' @param replace A named list of parameters to replace defaults.
#' If \code{NULL}, defaults will be used.
#'
#' @name make_parameters
#'
#' @encoding UTF-8

NULL

#' make_leafpar
#' @rdname make_parameters
#'
#' @inheritParams photosynthesis
#'
#' @return
#'
#' \code{make_leafpar}: An object inheriting from class \code{\link{leaf_par}}\cr
#' \code{make_enviropar}: An object inheriting from class \code{\link{enviro_par}}\cr
#' \code{make_bakepar}: An object inheriting from class \code{\link{bake_par}}\cr
#' \code{make_constants}: An object inheriting from class \code{\link{constants}}
#'
#' @details
#'
#' \bold{Constants:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "constants", !tealeaves)
#' ```
#' 
#' \bold{Baking (i.e. temperature response) parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "bake", !tealeaves)
#' ```
#' 
#' \bold{Environment parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "enviro", !tealeaves)
#' ```
#'
#' \bold{Leaf parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "leaf", !tealeaves, 
#'                             is.na(note) | note != "optional")
#' ```
#'
#' If \code{use_tealeaves = TRUE}, additional parameters are:
#'
#' \bold{Constants:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "constants", tealeaves)
#' ```
#' 
#' \bold{Baking (i.e. temperature response) parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "bake", tealeaves)
#' ```
#' 
#' \bold{Environment parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "enviro", tealeaves)
#' ```
#'
#' \bold{Leaf parameters:}
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "leaf", tealeaves)
#' ```
#'
#' \bold{Optional leaf parameters:}
#'
#' ```{r, echo=FALSE}
#'  make_photo_parameter_table(type == "leaf", note == "optional")
#' ```
#' 
#' \bold{stuff in original I might want to emulate:}
#' \tabular{lllll}{
#' \eqn{Sh} \tab \code{Sh} \tab Sherwood number \tab none \tab \link[=.get_sh]{calculated} \cr
#' \eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated}
#' }
#' @references
#'
#' Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic
#' rate into contributions from different variables. Plant, Cell & Environment
#' 38: 1200-11.
#'
#' @examples
#' bake_par = make_bakepar()
#' constants = make_constants(use_tealeaves = FALSE)
#' enviro_par = make_enviropar(use_tealeaves = FALSE)
#' leaf_par = make_leafpar(use_tealeaves = FALSE)
#'
#' leaf_par = make_leafpar(
#'   replace = list(
#'     g_sc = set_units(0.3, "umol/m^2/s"),
#'     V_cmax25 = set_units(100, "umol/m^2/s")
#'   ), use_tealeaves = FALSE
#' )
#' @export
#' @md

make_leafpar = function(
    replace = NULL, 
    use_tealeaves
  ) {

  # Message about new conductance model ----
  message_experimental(replace)

  # Defaults -----
  obj = make_default_parameter_list(
    which = "leaf",
    use_tealeaves = use_tealeaves
  )

  # Replace defaults ----
  if (!is.null(replace$T_leaf) & use_tealeaves) {
    warning("replace$T_leaf ignored when use_tealeaves is TRUE")
    replace$T_leaf = NULL
  }

  par_equiv = data.frame(
    tl = c("g_sw", "g_uw", "logit_sr"),
    ph = c("g_sc", "g_uc", "k_sc")
  )

  if (any(purrr::map_lgl(replace[par_equiv$tl], ~ !is.null(.x)))) {
    par_equiv %>%
      dplyr::filter(.data$tl %in% names(replace)) %>%
      dplyr::transmute(message = stringr::str_c(
        "\nIn `replace = list(...)`,
             tealeaves parameter ", .data$tl, " is not replacable. Use ",
        .data$ph, " instead."
      )) %>%
      dplyr::pull(.data$message) %>%
      stringr::str_c(collapse = "\n") %>%
      stop(call. = FALSE)
  }

  obj %<>% replace_defaults(replace)

  # Assign class and return ----
  obj %<>% photosynthesis::leaf_par(use_tealeaves)

  obj
}

#' make_enviropar
#' @rdname make_parameters
#' @export

make_enviropar = function(replace = NULL, use_tealeaves) {

  # Defaults ----
  obj = make_default_parameter_list(
    which = "enviro", 
    use_tealeaves = use_tealeaves
  )

  # Replace defaults ----
  if ("T_sky" %in% names(replace)) {
    if (is.function(replace$T_sky)) {
      obj$T_sky = replace$T_sky
      replace$T_sky = NULL
    }
  }

  par_equiv = data.frame(
    tl = c("S_sw"),
    ph = c("PPFD"),
    stringsAsFactors = FALSE
  )

  if (any(purrr::map_lgl(replace[par_equiv$tl], ~ !is.null(.x)))) {
    par_equiv %>%
      dplyr::filter(.data$tl %in% names(replace)) %>%
      dplyr::transmute(message = stringr::str_c(
        "\nIn `replace = list(...)`,
             tealeaves parameter ", .data$tl,
        " is not replacable. Use ", .data$ph, " instead."
      )) %>%
      dplyr::pull(.data$message) %>%
      stringr::str_c(collapse = "\n") %>%
      stop(call. = FALSE)
  }

  obj %<>% replace_defaults(replace)

  # Assign class and return ----
  obj %<>% photosynthesis::enviro_par(use_tealeaves)

  obj
}

#' make_bakepar
#' @rdname make_parameters
#' @export

make_bakepar = function(replace = NULL) {

  # Defaults -----
  obj = make_default_parameter_list(which = "bake", use_tealeaves = FALSE)

  # Replace defaults -----
  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% photosynthesis::bake_par()

  obj
  
}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants = function(replace = NULL, use_tealeaves) {

  # Defaults -----
  obj = make_default_parameter_list(
    which = "constants", 
    use_tealeaves = use_tealeaves
  )

  # Replace defaults -----
  if ("nu_constant" %in% names(replace)) {
    stopifnot(is.function(replace$nu_constant))
    obj$nu_constant = replace$nu_constant
    replace$nu_constant = NULL
  }

  if ("sh_constant" %in% names(replace)) {
    stopifnot(is.function(replace$sh_constant))
    obj$sh_constant = replace$sh_constant
    replace$sh_constant = NULL
  }

  obj %<>% replace_defaults(replace)

  # Assign class and return -----
  obj %<>% photosynthesis::constants(use_tealeaves)

  obj
}

#' Character vector of acceptable parameter types
#' @noRd
get_par_types = function() {
  c("bake", "constants", "enviro", "leaf")
}

#' Make default parameter list
#' @inheritParams parameter_names
#' @noRd
make_default_parameter_list = function(which, use_tealeaves) {
  
  which = which |>
    match.arg(get_par_types())
  
  default_parameter_list = photo_parameters |>
    dplyr::filter(
      type == which, 
      !temperature_response,
      ifelse(!use_tealeaves, !tealeaves, TRUE)
    ) |>
    dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
    split(~ R) |>
    purrr::map(function(.x) {
      if(is.na(.x$default)) {
        a = numeric(0)
        units(a) = as_units(.x$units)
        return(a)
      } else {
        units(.x$default) = as_units(.x$units)
        return(.x$default)
      }
    })
  
  # Hack for parameters that are functions
  if (which == "constants") {
    default_parameter_list %<>% c(
      nu_constant = function(Re, type, T_air, T_leaf, surface, unitless) {
        if (!unitless) {
          stopifnot(units(T_air)$numerator == "K" &
                      length(units(T_air)$denominator) == 0L)
          stopifnot(units(T_leaf)$numerator == "K" &
                      length(units(T_leaf)$denominator) == 0L)
        }
        
        type %<>% match.arg(c("free", "forced"))
        
        if (identical(type, "forced")) {
          if (unitless) {
            if (Re <= 4000) ret = list(a = 0.6, b = 0.5)
            if (Re > 4000) ret = list(a = 0.032, b = 0.8)
          } else {
            if (Re <= set_units(4000)) ret = list(a = 0.6, b = 0.5)
            if (Re > set_units(4000)) ret = list(a = 0.032, b = 0.8)
          }
          return(ret)
        }
        
        if (identical(type, "free")) {
          surface %<>% match.arg(c("lower", "upper"))
          if ((surface == "upper" & T_leaf > T_air) |
              (surface == "lower" & T_leaf < T_air)) {
            ret = list(a = 0.5, b = 0.25)
          } else {
            ret = list(a = 0.23, b = 0.25)
          }
          return(ret)
        }
      },
      sh_constant = function(type, unitless) {
        type %>%
          match.arg(c("free", "forced")) %>%
          switch(forced = 0.33, free = 0.25)
      }
    )
  }
    
  if (which == "enviro" & use_tealeaves) {
    default_parameter_list %<>% c(T_sky = function(pars) {
      set_units(pars$T_air, K) - set_units(20, K) * 
        set_units(pars$S_sw, W / m^2) / set_units(1000, W / m^2)
    })
  }
      
 default_parameter_list
 
}

#' Set parameter units
#' @param .x list of parameters to set units
#' @param ... arguments passed to dplyr::filter()
#' @noRd
set_parameter_units = function(.x, ...) {
  
  photo_parameters |>
    dplyr::filter(...) |>
    dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
    split(~ R) |>
    purrr::map(function(.y) {
      a = .x[[.y$R]]
      units(a) = as_units(.y$units)
      a    
    })
  
}

#' Message about experimental parameters
#' @inheritParams replace
#' @noRd
message_experimental = function(replace) {
  experimental_leafpar = c(
    "delta_ias_lower",
    "delta_ias_upper",
    "A_mes_A",
    "g_liqc"
  )
  if (any(names(replace) %in% experimental_leafpar)) {
    message(
      "
      It looks like you are using the new CO2 conductance model.
      
      As of version 2.1.0, the new CO2 conductance model is experimental and
      may change in new releases. Use with caution.
      ")
  }
  invisible()
}

#' Replace default parameters
#'
#' @param obj List of default values
#' @param replace List of replacement values
#' @noRd

replace_defaults = function(obj, replace) {
  if (!is.null(replace)) {
    stopifnot(is.list(replace))
    stopifnot(all(sapply(replace, inherits, what = "units")))
    stopifnot(all(sapply(replace, is.numeric)))
    x = names(replace)
    if (any(!x %in% names(obj))) {
      warning(sprintf("The following parameters in 'replace' were not
                      recognized:\n%s", paste0(x[!x %in% names(obj)],
        collapse = "\n"
      )))
      x %<>% .[. %in% names(obj)]
    }
    obj[x] = replace[x]
  }

  obj
}

