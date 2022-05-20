#' Replace in File
#'
#' Function to replace a specific text string in a text file. Useful to
#' manipulate GAMS sourcecode files.
#'
#'
#' @param file a connection object or a character string describing the file,
#' that should be manipulated.
#' @param manipulations A list of 2 element vectors, containing the search
#' phrase as first element and the replace term as second element.
#' @param perl usually set to TRUE so regular expressions in perl syntax (including backreferencing)
#' can be used. If fixed = TRUE is specified in ..., perl is set to FALSE
#' @param ... Further options passed to gsub
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{manipulateConfig}}
#' @examples
#'
#' # manipulateFile("example.txt",list(c("bla","blub"),c("a","b")))
manipulateFile <- function(file, manipulations, perl = TRUE, ...) {
  if (!is.list(manipulations)) {
    manipulations <- list(manipulations)
  }
  if ("fixed" %in% names(list(...)) && isTRUE(list(...)$fixed)) perl <- FALSE
  f <- paste(readLines(file), collapse = "\n")
  for (m in manipulations) {
    f <- gsub(m[1], m[2], f, perl = perl, ...)
  }
  writeLines(f, file)
}
