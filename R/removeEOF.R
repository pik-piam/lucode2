#' add EOF comment
#'
#' remove EOF text from all files in the code
#'
#' @param path path to the main folder of the model
#' @param filetypes file types the function should be applied to
#' @author Anastasis Giannousakis
#' @export
#' @seealso \code{\link{addEOF}}
#' @examples
#' \dontrun{
#' removeEOF()
#' }
#'
removeEOF <- function(path = ".", filetypes = c("inc", "prn", "gms")) {
  pattern <- paste("\\.(", paste(filetypes, collapse = "|"), ")$", sep = "")
  allFiles <- list.files(path = path, pattern = pattern, recursive = TRUE)

  for (i in seq_along(allFiles)) {
    values <- suppressWarnings(readLines(allFiles[i]))
    if (grepl("EOF", values[length(values)])) {
      values[length(values)] <- ""
      writeLines(values, allFiles[i])
    }
  }
}
