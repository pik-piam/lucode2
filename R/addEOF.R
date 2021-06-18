#' add EOF comment
#'
#' Add EOF text to all files in the code in order to ease debugging
#'
#'
#' @param path path to the main folder of the model
#' @param filetypes file types the function should be applied to
#' @author Anastasis Giannousakis
#' @seealso \code{\link{removeEOF}}
#' @export
#' @examples
#' \dontrun{
#' addEOF()
#' }
#'
addEOF <- function(path = ".", filetypes = c("inc", "prn", "gms")) {
  pattern <- paste("\\.(", paste(filetypes, collapse = "|"), ")$", sep = "")
  allFiles <- list.files(path = path, pattern = pattern, recursive = TRUE)

  for (i in seq_along(allFiles)) {
    expr <- paste("*** EOF ", allFiles[i], sep = "")
    cat(expr, file = allFiles[i], append = TRUE)
  }
}
