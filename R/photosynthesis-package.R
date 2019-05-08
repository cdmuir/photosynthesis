#' \code{photosynthesis} package
#' 
#' Model C3 Photosynthesis
#'
#' See the README on
#' \href{https://github.com/cdmuir/photosynthesis}{GitHub}
#'
#' @docType package
#' @name photosynthesis-package
#' @importFrom magrittr %>% %<>%
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom stats optim plogis
#' @importFrom units drop_units set_units
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c("degreeC", "g", "hPa", "J", "K", "kg", "kJ", "kPa", 
                         "m", "mol", "Pa", "PPFD", "s", "umol", "W"))