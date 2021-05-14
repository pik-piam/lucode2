#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files.
#'
#' @param filesToAutoFormat A character vector of paths to files that should be auto-formatted.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}
#' @importFrom styler style_file
#' @examples
#' \dontrun{
#' lucode2::autoFormat()
#' }
#' @export
autoFormat <- function(filesToAutoFormat = getFilesToLint()) {
  sapply(filesToAutoFormat, style_file)
}
