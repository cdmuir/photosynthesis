#' \code{photosynthesis} package
#'
#' Tools for Plant Ecophysiology & Modeling
#'
#' See the README on
#' \href{https://github.com/cdmuir/photosynthesis}{GitHub}
#'
#' @docType package
#' @name photosynthesis-package
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>% %<>%
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom stats coef optim plogis resid
#' @importFrom units as_units drop_units set_units
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c(
  ".A", ".Q", ".Qabs", "degreeC", "g", "hPa", "J", "K", "kg", "kJ", "kPa",
  "m", "mol", "normal", "Pa", "PPFD", "s", "umol", "W"
))
