#' Checking dependencies of photosynthesis
#'
#' @return check_dependencies returns an html report
#' that shows dependencies between functions and other
#' packages. Does not take any arguments to run. It is
#' useful for determining which functions need to be
#' changed together.
#'
#' @importFrom pkgnet CreatePackageReport
#' @export
#'
#' @examples
#' \donttest{
#' check_dependencies()
#' }
check_dependencies <- function() {
  # This function from pkgnet creates an html report and
  # opens the page within a browser.
  CreatePackageReport(pkg_name = "photosynthesis")
}
