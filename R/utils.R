#' Convert pressure from PPM to Pascals
#'
#' @param ppm Pressure value in umol/mol of class \code{units}
#' @param P Atmospheric pressure value in kPa of class \code{units}
#'
#' @return Value in Pa of class \code{units}
#'
#' @details
#'
#' \deqn{\mathrm{Press}(kPa) = \mathrm{Press}(ppm) P(kPa)}{Press(kPa) = Press(ppm) P(kPa)}
#' \deqn{\mathrm{Press}(Pa) = 1000 \mathrm{Press}(kPa)}{Press(Pa) = 1000 Press(kPa)}
#'
#' @examples
#'
#' ppm = set_units(400, "umol/mol")
#' P = set_units(101.325, "kPa")
#' ppm2pa(ppm, P)
#' @export
#'

ppm2pa = function(ppm, P) {
  set_units(ppm * P, Pa)
}

#' Convert number to scientific notation in LaTeX or R documentation
#' 
#' @param x vector of numbers to convert
#' @param .threshold integer threshold for order of magnitude to use scientific notation
#' @param .digits integer number of significant digits
#' @param .format character indicating whether to format output for LaTeX "latex" or R documentation "r"
#' 
#' @noRd
scientize = function(x, .threshold = -1L, .digits = 2L, .format = "r") {
  
  purrr::map_chr(x, function(.x, .threshold, .digits, .format) {
    
    if (is.na(.x)) {
      return(NA) 
    } else {
      oom = log10(abs(.x))
      if (oom < .threshold) {
        x1 = .x |>
          magrittr::multiply_by(10 ^ -floor(oom)) |>
          round(.digits)
        
        x2 = sprintf(glue::glue("%.{.digits}f"), x1) |>
          stringr::str_c("\\times 10^{", floor(oom), "}") 
        
        if (.format == "r") {
          x2 = glue::glue("\\eqn{<x2>}", .open = "<", .close = ">")
        }
        
        return(x2)
      } else {
        x1 = .x |>
          signif(.digits + 1L) |>
          as.character()
        return(x1)
      }
    }
  }, .threshold = .threshold, .digits = .digits, .format = .format)
  
}

#' Make table for R documentation on photosynthesis parameters
#' 
#' @param ... arguments passed to dplyr::filter()
#' 
#' @noRd
make_photo_parameter_table = function(...) {
  
  photosynthesis::photo_parameters |>
    dplyr::filter(...) |>
    dplyr::mutate(
      Symbol = glue::glue("\\eqn{<symbol>}", .open = "<", .close = ">"),
      R = glue::glue("\\code{<R>}", .open = "<", .close = ">"),
      Units = stringr::str_replace_all(units, "([A-Za-z]+)\\^([0-9]+)",
                                       "\\1\\\\eqn{^\\2}"),
      Default = scientize(.data$default, .threshold = -2L, .format = "r")
    ) |>
    dplyr::select(.data$Symbol, .data$R, Description = .data$description, 
                  .data$Units, .data$Default) |>
    knitr::kable()
  
}