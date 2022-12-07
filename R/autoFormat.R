#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files. Does not change indentation.
#'
#' @param files A character vector of paths to files that should be auto-formatted.
#' @param ignoreLintFreeFiles If set to TRUE (the default) files without linter warnings are not auto-formatted.
#' @param lintAfterwards If set to TRUE (the default) return linter results for the auto-formatted files.
#' @param formatTF If set to TRUE (the default is FALSE) ask for T and F to be converted into TRUE and FALSE.
#' Note that it currently can't find out reliably if T or F are aliases for TRUE and FALSE, so manual intervention
#' is needed.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}
#' @importFrom styler style_file
#' @examples
#' \dontrun{
#' lucode2::autoFormat()
#' }
#' @export
autoFormat <- function(files = getFilesToLint(), ignoreLintFreeFiles = TRUE, lintAfterwards = TRUE, formatTF = FALSE) {
  if (ignoreLintFreeFiles) {
    # keep only files with linter warnings
    files <- files[vapply(files, function(aFile) length(lint(aFile)) > 0, logical(1))]
  }

  if (formatTF) {
      lapply(files, askFormatTF)
  }

  # strict = FALSE -> keep alignment spaces around assignments, keep extra newlines
  # scope is set to not change indentation, otherwise the alignment of continuation lines would be messed up
  style_file(files, strict = FALSE, scope = I(c("tokens", "line_breaks", "spaces")))
  message("Hint: In RStudio you can fix indentation in selected lines using ctrl + i.")

  if (lintAfterwards) {
    print(lint(files))
  }
}

askFormatTF <- function(pathToFile) {
    fileContent <- readLines(pathToFile)
    if (any(grepl("\\b[TF]\\b", fileContent))) {
      newFileContent <- vapply(fileContent, function(originalLine) {
        if (!grepl("\\b[TF]\\b", originalLine)) {
          return(originalLine)
        }

        replacement <- gsub("\\bT\\b", "TRUE", originalLine)
        replacement <- gsub("\\bF\\b", "FALSE", replacement)
        message("original line:\n", originalLine, "\nreplacement:\n", replacement, "\n",
                "Replace? (Y/n) ", appendLF = FALSE)
        if (tolower(getLine()) %in% c("", "y", "yes")) {
          return(replacement)
        } else {
          return(originalLine)
        }
      }, character(1))
      writeLines(newFileContent, pathToFile)
    }
}
