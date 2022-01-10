#' extended Print
#'
#' An extended print command which formats information in a way that it is good
#' to use for a log-file
#'
#'
#' @param varName name of the variable that should be printed as string
#' @param envir environment from which the variable should be read (by default
#' the environment from which the function is called)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{eprint_list}}
#' @examples
#' \dontrun{
#' a <- 1:3
#' eprint("a")
#' }
#'
#' ### print additional information concerning loaded configuration###
#' ### ePrint (extended Print) offers an extended output functionality which
#' ### allows to create easily log-files with all relevant information
eprint <- function(varName, envir = parent.frame()) {
  varValue <- try(get(varName, envir = envir), silent = TRUE)
  if (class(varValue) == "try-error") {
    cat(paste(varName, "not found\n"))
  } else {
    cat(paste(varName, "<-", varValue, "\n"))
  }
}
