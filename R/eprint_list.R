#' Extended list print
#'
#' Same as \code{\link{eprint}}, but expecting a vector with variable names
#'
#'
#' @param var_list Vector containing names of several variables that should be
#' printed
#' @param envir environment from which the variable should be read (by default
#' the environment from which the function is called)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{eprint}}
#' @examples
#'
#' a <- 1:3
#' b <- "blub"
#' lucode2:::eprint_list(c("a", "b"))
eprint_list <- function(var_list, envir = parent.frame()) {
  for (varName in var_list) {
    eprint(varName, envir = envir)
  }
}
