#' formatTF
#'
#' Convert T and F to TRUE and FALSE, respectively. The tool finds T and F and asks about
#' each ocurrence if it should be converted. Please check carefully before agreeing to the
#' conversion, there are false positives e.g. if a dataframe column name is T or F.
#'
#' @param pathToFile Path to the R source file where T and F should be renamed.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{autoFormat}}, \code{\link{snakeToCamel}}
#' @examples
#' \dontrun{
#' lucode2::formatTF("myFile.R")
#' }
#' @export

formatTF <- function(pathToFile) {
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
