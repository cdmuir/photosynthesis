#' Compiling outputs from lists
#'
#' @param data List of elements
#' @param output_type Type of desired output. For graphs or models, use "list",
#' for parameters, use "dataframe".
#' @param list_element Which elements of the sublists do you wish to compile?
#'
#' @return compile_data converts the outputs of fit_many into a form more
#' readily usable for analysis. Can be used to create dataframe of all
#' fitted parameters, a list of model outputs, a list of graphs for plotting.
#' This function is NOT restricted to compiling outputs from plantecophystools
#' but could be used to compile elements from ANY list of lists.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Read in your data
#' # Note that this data is coming from data supplied by the package
#' # hence the complicated argument in read.csv()
#' # This dataset is a CO2 by light response curve for a single sunflower
#' data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#'   package = "photosynthesis"
#' ))
#'
#' # Define a grouping factor based on light intensity to split the ACi
#' # curves
#' data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#'
#' # Convert leaf temperature to K
#' data$T_leaf <- data$Tleaf + 273.15
#'
#' # Fit many curves
#' fits <- fit_many(
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
#' # Compile graphs into a list for plotting
#' fits_graphs <- compile_data(fits,
#'   list_element = 2
#' )
#'
#' # Plot one graph from the compiled list
#' plot(fits_graphs[[1]])
#' }
compile_data <- function(
    data,
    output_type = "list",
    list_element
  ) {
  # Is output_type compatible with options?
  if (!output_type %in% c("list", "dataframe")) {
    stop("Output type not found. Use list or dataframe.")
  }
  # Create empty list
  output <- vector("list", length(data))
  # Create output list with desired elements
  # Add correct names
  for (i in seq_along(data)) {
    output[[i]] <- data[[i]][[list_element]]
    names(output)[i] <- names(data[i])
  }
  # If desired output is a list, return output list here
  if (output_type == "list") {
    return(output)
  }
  # If desired output is a dataframe, create it from the list here
  # Add ID column to dataframe
  if (output_type == "dataframe") {
    for (i in 1:length(output)) {
      output[[i]]$ID <- names(output)[i]
    }
    output <- do.call("bind_rows", output)
    return(output)
  }
}
