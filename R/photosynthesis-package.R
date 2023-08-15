#' `photosynthesis` package
#'
#' Tools for Plant Ecophysiology & Modeling
#'
#' See the README on
#' [GitHub](https://github.com/cdmuir/photosynthesis)
#'
#' @docType package
#' @name photosynthesis-package
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>% %<>%
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom methods is
#' @importFrom nlme lmList
#' @importFrom rlang .data
#' @importFrom stats coef lm optim plogis resid rnorm
#' @importFrom units as_units drop_units set_units
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c(
  ".A", ".C", ".phiPSII", ".Q", ".Qabs", "degreeC", "g", "hPa", "J", "K", "kg", 
  "kJ", "kPa", "m", "mol", "mmol", "normal", "Pa", "PPFD", "s", "umol", "W"
))

## quiets concerns of R CMD check about using ::: operator
.get_Dx <- utils::getFromNamespace(".get_Dx", "tealeaves")
.get_gbw <- utils::getFromNamespace(".get_gbw", "tealeaves")
.get_ps <- utils::getFromNamespace(".get_ps", "tealeaves")
