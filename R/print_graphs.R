#' Printing graphs to system
#'
#' @param data List of graphs
#' @param path File path for printing our graphs. Use "./" to set to current
#' working directory
#' @param output_type Type of output file, jpeg or pdf
#' @param height Height of jpegs
#' @param width Width of jpegs
#' @param res Resolution of jpegs
#' @param units Units of height and width
#' @param pdf_filename Filename for pdf option
#' @param ... Further arguments for jpeg() and pdf()
#'
#' @return print_graphs creates graph files in current working directory
#' from a list of graphs
#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom grDevices pdf
#' @importFrom graphics par
#' @importFrom graphics plot
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
#' # Fit many AQ curves
#' # Set your grouping variable
#' # Here we are grouping by CO2_s and individual
#' data$C_s <- (round(data$CO2_s, digits = 0))
#'
#' # For this example we need to round sequentially due to CO2_s setpoints
#' data$C_s <- as.factor(round(data$C_s, digits = -1))
#'
#' # To fit one AQ curve
#' fit <- fit_aq_response(data[data$C_s == 600, ],
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin"
#'   )
#' )
#'
#' # Print model summary
#' summary(fit[[1]])
#'
#' # Print fitted parameters
#' fit[[2]]
#'
#' # Print graph
#' fit[[3]]
#'
#' # Fit many curves
#' fits <- fit_many(
#'   data = data,
#'   varnames = list(
#'     A_net = "A",
#'     PPFD = "Qin",
#'     group = "C_s"
#'   ),
#'   funct = fit_aq_response,
#'   group = "C_s"
#' )
#'
#' # Look at model summary for a given fit
#' # First set of double parentheses selects an individual group value
#' # Second set selects an element of the sublist
#' summary(fits[[3]][[1]])
#'
#' # Print the parameters
#' fits[[3]][[2]]
#'
#' # Print the graph
#' fits[[3]][[3]]
#'
#' # Compile graphs into a list for plotting
#' fits_graphs <- compile_data(fits,
#'   list_element = 3
#' )
#'
#' # Print graphs to pdf
#' # Uncomment to run
#' # print_graphs(data = fits_graphs,
#' #            output_type = "pdf",
#' #            path = tempdir(),
#' #            pdf_filename = "mygraphs.pdf")
#' }
print_graphs <- function(data,
                         path,
                         output_type = "jpeg",
                         height = 5,
                         width = 5,
                         res = 600,
                         units = "in",
                         pdf_filename,
                         ...) {
  # Is output_type compatible with options?
  if (!output_type %in% c("pdf", "jpeg")) {
    stop("Output type not found. Use pdf or jpeg")
  }
  if (!missing(path)) {
    # Print out individual jpeg files
    if (output_type == "jpeg") {
      for (i in 1:length(data)) {
        jpeg(paste(names(data[i])[1], ".jpeg"),
          height = height, width = width,
          res = res, units = units, ...
        )
        print(data[[i]])
        dev.off()
      }
    }
    # Print out pdf with all graphs
    if (output_type == "pdf") {
      pdf(pdf_filename, ...)
      old_mfrow = par()$mfrow
      par(mfrow = c(2, 2))
      on.exit(par(mfrow = old_mfrow))
      for (i in 1:length(data)) {
        plot(data[[i]], main = names(data[i]))
      }
      dev.off()
    }
  } else {
    message("Graphs not printed. 'path' argument required.")
  }
}
