#' Fitting many functions across groups
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' We are no longer updating this function. Please use generic methods like \code{\link[purrr]{map}} instead. See `vignette("light-response")` for an example.
#' 
#' @param data Dataframe
#' @param funct Function to fit
#' @param group Grouping variables
#' @param progress Flag. Show progress bar?
#' @param ... Arguments for the function to fit. Use ?functionname
#' to read the help file on available arguments for a given function.
#'
#' @return fit_many fits a function across every instance of
#' a grouping variable.
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @export
#'
#' @examples
#' \donttest{
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data = read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Define a grouping factor based on light intensity to split the ACi
#' # curves
#' data$Q_2 = as.factor((round(data$Qin, digits = 0)))
#'
#' # Convert leaf temperature to K
#' data$T_leaf = data$Tleaf + 273.15
#'
#' # Fit many curves
#' fits = fit_many(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     T_leaf = "T_leaf",
#'     C_i = "Ci",
#'     PPFD = "Qin"
#'   ),
#'   funct = fit_aci_response,
#'   group = "Q_2"
#' )
#'
#' # Print the parameters
#' # First set of double parentheses selects an individual group value
#' # Second set selects an element of the sublist
#' fits[[3]][[1]]
#'
#' # Print the graph
#' fits[[3]][[2]]
#'
#' # Compile graphs into a list for plotting
#' fits_graphs = compile_data(fits,
#'   list_element = 2
#' )
#'
#'
#' # Compile parameters into dataframe for analysis
#' fits_pars = compile_data(fits,
#'   output_type = "dataframe",
#'   list_element = 1
#' )
#' }
#' 
#' @md

fit_many = function(
    data,
    funct,
    group,
    progress = TRUE,
    ...
) {
  
  lifecycle::deprecate_soft("2.1.3", "fit_many()")
  
  checkmate::assert_flag(progress)
  
  # Split data into list by group
  data = split(data, data[, group])

  # Create empty list by group
  fits = list(NULL)

  # Start progress bar
  if (progress) {
    pb = txtProgressBar(min = 0, max = length(data), style = 3)
  }
  
  # Loop through list, fitting the function
  for (i in 1:length(data)) {
    fits[[i]] = funct(data[[i]], ...)
    names(fits)[i] = names(data[i])
    # Set progress bar
    if (progress) setTxtProgressBar(pb, i)
  }
  # Return the list of fits
  return(fits)
}
