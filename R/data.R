#' Input parameters to simulate C3 photosynthesis using [photosynthesis()]
#'
#' A table of input parameters used in [photosynthesis()] 
#'
#' @format ## `photo_parameters`
#' A data frame with `r nrow(photo_parameters)` rows and `r ncol(photo_parameters)`  columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"photo_parameters"
