#' setwd2
#'
#' Mini function that allows you to set a directory based on a readline input.
#' Very useful for Windows users, as it replaces backslashes by slashes.
#'
#' @param return_only if TRUE, the path is not changed, but the clipboard path is returned as string.
#' @return if return_only=FALSE: Nothing, but the working directory is set to. Otherwise: no working directory returned,
#'         but path transformed.
#' @author Benjamin Leon Bodirsky
#' @export
setwd2 <- function(return_only = FALSE) { # nolint
  cat("Please enter URL here:")
  x <- readline()
  x <- gsub("\\\\", "/", x)
  if (return_only == FALSE) {
    setwd(x) # nolint
  } else {
    return(x)
  }
}
