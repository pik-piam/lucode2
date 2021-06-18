#' path
#'
#' Small function to build a consistent path-string based on folder, filename
#' and filetype. The function makes sure that slashes and the dot for the file
#' ending are set correctly (you can supply your folder name either with or
#' without a tailing slash in it. It does not matter.
#'
#' @aliases path path
#' @param ... the folders and the file name that should be pasted to a
#' file/folder path
#' @param ftype file type
#' @return A string containing the path combined of folder, filename and
#' filetype
#' @author Jan Philipp Dietrich
#' @export
path <- function(..., ftype = NULL) {
  .Deprecated("file.path")
  if (!is.null(ftype)) if (substring(ftype, 1, 1) != ".") ftype <- paste(".", ftype, sep = "")
  out <- paste(..., sep = "/")
  out <- paste(gsub("//", "/", out), ftype, sep = "")
  first <- list(...)[[1]]
  .tmp <- function(first, out) {
    if (is.null(first) || first == "") {
      out <- gsub("^/+", "", out)
    }
    return(out)
  }
  if (length(first) > 1) {
    for (i in seq_along(first)) {
      out[i] <- .tmp(first[i], out[i])
    }
  } else {
    out <- .tmp(first, out)
  }
  return(out)
}
