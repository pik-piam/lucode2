#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files.
#'
#' @param files A character vector of paths to files that should be auto-formatted.
#' @param ignoreLintFreeFiles If set to TRUE only files with linter warnings are auto-formatted.
#' @param lintAfterwards If set to TRUE return linter results of the auto-formatted files.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}
#' @importFrom styler style_file
#' @examples
#' \dontrun{
#' lucode2::autoFormat()
#' }
#' @export
autoFormat <- function(files = getFilesToLint(), ignoreLintFreeFiles = TRUE, lintAfterwards = TRUE) {
  if (ignoreLintFreeFiles) {
    files <- files[sapply(files, function(aFile) length(lint(aFile)) > 0)]
  }
  style_file(files)
  if (lintAfterwards) {
    return(lint(files))
  }
}
