#' extended Print
#'
#' An extended print command which formats information in a way that it is good
#' to use for a log-file
#'
#'
#' @param var_name name of the variable that should be printed as string
#' @param envir environment from which the variable should be read (by default
#' the environment from which the function is called)
#' @author Jan Philipp Dietrich, Oliver Richters
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
eprint <- function(var_name, envir = parent.frame()) { # nolint
  varValue <- try(get(var_name, envir = envir), silent = TRUE)
  if (inherits(varValue, "try-error")) {
    message(paste(var_name, "not found"))
  } else {
    message(paste(var_name, "<-", paste(varValue, collapse = ", ")))
  }
}
