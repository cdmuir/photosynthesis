#' Read a LI-COR file
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' We are no longer updating this function. Please use \code{\link{read_licor}} instead.
#' 
#' @param x File name
#'
#' @return Returns a data.frame from raw LI-COR files. Current support
#' for LI-COR LI-6800 files only. 
#' @importFrom utils read.csv
#'
#' @md
#' @export
read_li6800 = function(x) {
  
  lifecycle::deprecate_soft("2.1.3", "read_li6800()", with = "read_licor()")
  
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

#' Read a LI-COR file
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Reads a raw LI-COR LI6800 file, including remarks. This function was
#' developed using output from Bluestem v.2.0.04 to v.2.1.08. We cannot
#' guarantee backward compatibility with earlier versions of Bluestem. We will 
#' try to update code when new versions are released, but there maybe a 
#' time-lag, so inspect results carefully.
#' 
#' @param file Path to a raw LI6800 file
#' @param bluestem_version Character string of Bluestem software version number. By default, the function will try to pull the version number from file.
#' @param ... Argument passed to \code{\link[readr]{read_lines}}
#'
#' @return Returns a \code{\link[tibble]{tibble}} from raw LI-COR LI6800 files. 
#'
#' @export
read_licor = function(
    file,
    bluestem_version = get_bluestem_version(file, n_max = 10L),
    ...
) {
  
  v1 = "2.0.04"
  v2 = "2.1.08"
  checkmate::assert_string(bluestem_version)

  if (numeric_version(bluestem_version) < v1) {
    warning(glue::glue("It appears you are using data from Bluestem version {bluestem_version}. `read_licor()` function was developed with versions starting at {v1} and has not been tested with earlier versions. Inspect results carefully."))
  }
  
  if (numeric_version(bluestem_version) > v2) {
    warning(glue::glue("It appears you are using data from Bluestem version {bluestem_version}. `read_licor()` function was developed with versions up to {v2} and has not been tested with more recent versions. Inspect results carefully."))
  }
  
  # Read lines
  all_lines = readr::read_lines(file, ...)
  
  # Extract header information and covert to named list
  header = all_lines |>
    extract_licor_header() |>
    purrr::map(stringr::str_split_1, "\t") |>
    purrr::map(restructure_licor_header_line) %>%
    rlang::set_names(sapply(., `[`, 1L)) |>
    purrr::map(`[`, -1L)
  
  # Extract remarks and convert to tibble
  remarks = all_lines |>
    extract_licor_remarks() |>
    tibble::as_tibble() |>
    tidyr::separate(col = "value", into = c("time", "remark"), sep = "\t") 
  
  # Extract data and convert to a tibble
  data_block = setdiff(all_lines, extract_licor_remarks(all_lines))
  data_start_line = stringr::str_detect(data_block, "\\[Data\\]")
  var_names = stringr::str_split_1(data_block[which(data_start_line) + 2L],
                                   pattern = "\t")
  
  utils::read.table(
    text = data_block[(which(data_start_line) + 4L):length(data_block)],
    sep = "\t"
  ) |>
    magrittr::set_colnames(var_names) |>
    magrittr::set_attr("remarks", remarks) |>
    magrittr::set_attr("header", header)
    
}

#' Get Bluestem version from LI6800 file
#' 
#' @inheritParams read_licor
#' @param ... Argument passed to \code{\link[readr]{read_lines}}
#' @noRd
get_bluestem_version = function(file, ...) {
  x1 = readr::read_lines(file, ...)
  ver_number_string = "[0-9]+.[0-9]+.[0-9]+"
  ver_string = paste0("Console ver\tBluestem v.", ver_number_string)
  ver_line = which(stringr::str_detect(x1, ver_string))
  stringr::str_extract(x1[ver_line], ver_number_string)
}

#' Extract header table from a LI6800 raw file
#' @noRd
extract_licor_header = function(.x) {
  header_line = "\\[Header\\]"
  data_line = "\\[Data\\]"
  first_line = stringr::str_detect(.x, header_line) |>
    which() |>
    magrittr::add(1L)
  last_line = stringr::str_detect(.x, data_line) |>
    which() |>
    magrittr::subtract(1L)
  
  # Remove remarks
  header = .x[first_line:last_line]
  remarks = extract_licor_remarks(header)
  
  setdiff(header, remarks)
}

#' Extract remarks from a LI6800 raw file
#' @noRd
extract_licor_remarks = function(.x) {
  # Remark lines have time stamp, tab, and no more '\t' in remainder of line
  remark_line = "^[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\t(?!.*(\t))"
  .x[stringr::str_detect(.x, remark_line)]
}

#' Restructure LI6800 header rows
#' @description
#' Restructures header rows in raw LI6800 files as needed. Currently, it only alters the row with Stability Definition
#' @param header_line A character vector from one row in the header after splitting by tabs.
#' @noRd
restructure_licor_header_line = function(header_line) {
  checkmate::assert_character(header_line)
  
  # Restructure Stability Definition line
  ret = if (
    stringr::str_detect(header_line[1], "^[0-2][0-9]:[0-5][0-9]:[0-5][0-9]$") &
    stringr::str_detect(header_line[2], "^Stability Definition:$")
  ) {
    header_line[2:length(header_line)]
  } else{
    header_line
  }
  
  ret
  
}
