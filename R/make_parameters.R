#' Make lists of parameters for \code{photosynthesis}
#'
#' @param replace A named list of parameters to replace defaults.
#' If \code{NULL}, defaults will be used.
#'
#' @name make_parameters
#'
#' @encoding UTF-8

NULL

#' make_anypar
#' @inheritParams make_parameters
#' @inheritParams parameter_names
#' @noRd
make_anypar = function(which, replace, use_tealeaves) {

  which = match.arg(which, choices = get_par_types())
  
  # Message about new conductance model ----
  message_experimental(replace)
  
  # Defaults -----
  obj = make_default_parameter_list(
    which = which,
    use_tealeaves = use_tealeaves
  )
  
  # Special procedures for constants ---
  if (which == "constants") {
    
    if ("f_nu" %in% names(replace)) {
      stopifnot(is.function(replace$f_nu))
      obj$f_nu = replace$f_nu
      replace$f_nu = NULL
    }
    
    if ("f_sh" %in% names(replace)) {
      stopifnot(is.function(replace$f_sh))
      obj$f_sh = replace$f_sh
      replace$f_sh = NULL
    }
    
  }

  # Special procedures for enviro_par ----
  if (which == "enviro") {
    
    if ("T_sky" %in% names(replace)) {
      if (is.function(replace$T_sky)) {
        obj$T_sky = replace$T_sky
        replace$T_sky = NULL
      }
    }
    
    par_equiv = data.frame(
      tl = c("S_sw"),
      ph = c("PPFD")
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
    
  }
  
  # Special procedures for leaf_par ----
  if (which == "leaf" & use_tealeaves) {
    
    par_equiv = get_par_equiv()
    
    # Some equivalencies require additional parameters. Therefore leaving
    # those parameter values empty
    tl_placeholders = photosynthesis::photo_parameters |>
      dplyr::filter(.data$R %in% par_equiv$tl) |>
      dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
      split(~ R) |>
      purrr::map(function(.y) {
        a = 0
        units(a) = as_units(.y[["units"]])
        a
      })
    obj[names(tl_placeholders)] = tl_placeholders
    
    if (!is.null(replace)) {
      if (!is.null(replace$T_leaf)) {
        warning("replace$T_leaf ignored when use_tealeaves is TRUE")
        replace$T_leaf = NULL
      }
      
      
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
    }
  }
  
  # Replace defaults ----
  obj %<>% replace_defaults(replace)
  
  # Assign class and return ----
  switch(
    which,
    bake = photosynthesis::bake_par(obj),
    constants = photosynthesis::constants(obj, use_tealeaves),
    enviro = photosynthesis::enviro_par(obj, use_tealeaves),
    leaf = photosynthesis::leaf_par(obj, use_tealeaves),
  )

}
  
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
#'     g_sc = set_units(0.3, mol / m^2 / s),
#'     V_cmax25 = set_units(100, umol / m^2 / s)
#'   ), use_tealeaves = FALSE
#' )
#' @export
#' @md

make_leafpar = function(replace = NULL, use_tealeaves) {

  make_anypar("leaf", replace = replace, use_tealeaves = use_tealeaves)
  
}

#' make_enviropar
#' @rdname make_parameters
#' @export

make_enviropar = function(replace = NULL, use_tealeaves) {

  make_anypar("enviro", replace = replace, use_tealeaves = use_tealeaves)

}

#' make_bakepar
#' @rdname make_parameters
#' @export

make_bakepar = function(replace = NULL) {

  make_anypar("bake", replace = replace, use_tealeaves = FALSE)
  
}

#' make_constants
#' @rdname make_parameters
#' @export

make_constants = function(replace = NULL, use_tealeaves) {

  make_anypar("constants", replace = replace, use_tealeaves = use_tealeaves)
  
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
  
  default_parameter_list = photosynthesis::photo_parameters |>
    dplyr::filter(
      .data$type == which, 
      !.data$temperature_response,
      if (!use_tealeaves) {!.data$tealeaves} else TRUE,
    ) |>
    dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
    split(~ R) |>
    purrr::map(function(.x) {
      if(is.na(.x$default)) {
        if (.x$note == "optional") {
          a = numeric(0)
          units(a) = as_units(.x$units)
          return(a)
        }
        if (.x$note == "calculated") {
          get_f_parameter(.x$R)
        }
      } else {
        units(.x$default) = as_units(.x$units)
        return(.x$default)
      }
    })
  
  default_parameter_list
  
}

#' Check parameter names
#' @inheritParams set_parameter_units
#' @inheritParams parameter_names
#' @noRd
check_parameter_names = function(.x, which, use_tealeaves) {
  
  stopifnot(is.list(.x))
  
  nms = parameter_names(which, use_tealeaves = use_tealeaves)
  
  # Don't fail check if .x is missing tealeaves parameter equivalents
  nms1 = nms[!(nms %in% get_par_equiv()[, "tl"])]
  if (which == "leaf" & use_tealeaves) nms1 = nms1[!(nms1 == "T_leaf")]
  
  if (!all(nms1 %in% names(.x))) {
    nms1[!(nms1 %in% names(.x))] |>
      stringr::str_c(collapse = ", ") %>%
      glue::glue("{x} not in parameter names required for {which}",
                 x = ., which = which
      ) |>
      stop()
  }
  
  nms
  
}

#' Set parameter units
#' @param .x list of parameters to set units
#' @param ... arguments passed to dplyr::filter()
#' @noRd

set_parameter_units = function(.x, ...) {
  
  photosynthesis::photo_parameters |>
    dplyr::filter(...) |>
    dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
    split(~ R) |>
    purrr::map(function(.y) {
      v = .x[[.y$R]]
      if (is.function(v)) {
        v
      } else {
        a = if (is.null(v)) {0} else {v}
        units(a) = as_units(.y$units)
        a
      }
    })
  
}

#' Assert parameter bounds
#' @param .x list of parameters
#' @param ... arguments passed to dplyr::filter()
#' @noRd
assert_parameter_bounds = function(.x, ...) {
  
  photosynthesis::photo_parameters |>
    dplyr::filter(...) |>
    dplyr::mutate(units = stringr::str_replace(units, "none", "1")) |>
    split(~ R) |>
    purrr::map_lgl(function(.y) {
      if (
        length(.x[[.y$R]]) == 0L |
        is.function(.x[[.y$R]]) | 
        is.na(.y$lower) | 
        is.na(.y$upper)
      ) {
        TRUE
      } else {
        units(.y$lower) = as_units(.y$units)
        units(.y$upper) = as_units(.y$units)
        all(.x[[.y$R]] >= .y$lower & .x[[.y$R]] <= .y$upper)
      }
    }) |>
    all() |>
    checkmate::assert_true()
  
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

#' Get data.frame of equivalent parameters between tealeaves and photosynthesis
#' @noRd
get_par_equiv = function() {
  data.frame(
    tl = c("g_sw", "g_uw", "logit_sr"),
    ph = c("g_sc", "g_uc", "k_sc")
  )
}
