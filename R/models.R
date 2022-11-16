#' Get default model
#' 
#' @rdname models
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Get the name of the default model used for different plant ecophysiological data analysis methods implemented in \bold{photosynthesis}. Currently only used for \code{\link{fit_aq_response2}}.
#' 
#' @param method Character string. One of "aq_response"
#' 
#' @return A character string with name of model.
#' 
#' @example 
#' get_default_model("aq_response")
#' 
#' @md
get_default_model = function(method) {
  match.arg(method, "aq_response")
  switch(
    method,
    aq_response = "marshall_biscoe_1980"
  )
}

#' @rdname models
get_all_models = function(method) {
  match.arg(method, "aq_response")
  switch(
    method,
    aq_response = "marshall_biscoe_1980"
  )
}
