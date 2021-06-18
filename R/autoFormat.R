#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files. Does not change indentation.
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
    # keep only files with linter warnings
    files <- files[lapply(files, function(aFile) length(lint(aFile)) > 0)]
  }

  # strict = FALSE -> keep alignment spaces around assignments, keep extra newlines
  # scope is set to not change indentation, otherwise the alignment of continuation lines would be messed up
  style_file(files, strict = FALSE, scope = I(c("tokens", "line_breaks", "spaces")))
  cat("Hint: In RStudio you can fix indentation in selected lines using ctrl + i.\n")

  if (lintAfterwards) {
    return(lint(files))
  }
}
