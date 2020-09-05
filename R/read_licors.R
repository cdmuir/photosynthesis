#' Reading in LiCor files
#'
#' @param x File name
#'
#' @return Returns a dataframe from raw LiCor files. Current support
#' for LiCor 6800 files only. LiCor 6400 file reading will be supported
#' in a later version.
#' @importFrom utils read.csv
#'
#' @rdname read_licors
#' @export
read_li6800 <- function(x) {
  # Read in header information
  header <- read.csv(
    file = x, header = TRUE, sep = "\t",
    skip = grep(
      pattern = "\\[Data\\]",
      x = readLines(x),
      value = FALSE
    ) + 1,
    nrows = 1
  )
  # Read in data information
  data <- read.csv(
    file = x, header = FALSE, sep = "\t",
    skip = grep(
      pattern = "\\[Data\\]",
      x = readLines(x),
      value = FALSE
    ) + 3
  )
  # Add header to data
  colnames(data) <- colnames(header)
  # Return data
  return(data)
}
