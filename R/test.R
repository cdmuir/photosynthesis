#' test
#' 
#' @name test
#' 
#' @details
#'
#' The table is: 
#'
#' | Tables        | Are           | Cool  |
#' | ------------- |:-------------:| -----:|
#' | col 3 is      | \eqn{A}{A} | $1600 |
#' | col 2 is      | A      |   $12 |
#'
#' ```{r}
#' # This block of code will be evaluated
#' tibble::tibble(
#'  x = c("$A$", "\\eqn{A}{A}"),
#'  y = c("\\code{A}", "25 °C")
#' ) |>
#' knitr::kable()
#' ```

#' @md
test = function() {
  message("foo")
}
