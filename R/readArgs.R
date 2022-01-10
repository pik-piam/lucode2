#' Read Arguments from command line
#'
#' Function reads arguments from command line of the structure value=content
#' and transforms them to R-Values, if they are called as allowed arguments.
#'
#' @param \dots arguments allowed to be read from command line (other values
#' are ignored). Value is set if found on command line input, nothing is done,
#' if value is not found.
#' @param .envir environment in which the variables should be written (by
#' default the environment from which the function is called)
#' @param .silent boolean which allows to suppress status messages
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{manipulateConfig}}
#' @examples
#'
#' # Create an R-file "test.R" with following code:
#' value1 <- "old"
#' value2 <- 2
#' value3 <- "willstaythesame"
#' readArgs("value1", "value2", "value4")
#' cat(value1, "\n")
#' cat(value2, "\n")
#' cat(value3, "\n")
#'
#' # Open the command line and execute the following code:
#' # Rscript test.R value1=new value2=3 value3=isnotallowed
#'
#' # Output:
#' #
#' # ### READ COMMAND LINE - ASSIGNED CONFIGURATION ###
#' # value1 <- new
#' # value2 <- 3
#' # value4 not found
#' # ### READ COMMAND LINE - CONFIGURATION END ###
#' #
#' # "new"
#' # 3
#' # "willstaythesame"
#'
#'
#' ### function that reads all allowed arguments from command line###
readArgs <- function(..., .envir = parent.frame(), .silent = FALSE) {
  allowedArgs <- c(...)

  ### apply additional command line arguments###
  if (length(commandArgs()) > 5)
  for (argnr in 6:length(commandArgs())) {
    for (i in seq_along(allowedArgs)) {
      if (strsplit(commandArgs()[argnr], "=")[[1]][1] == allowedArgs[i]) {
        assign(allowedArgs[i], extract_arguments(commandArgs()[argnr]), envir = .envir)
      }
    }
  }
  if (!.silent) {
    cat("\n\n### READ COMMAND LINE - ASSIGNED CONFIGURATION ###\n")
    eprint_list(allowedArgs, envir = .envir)
    cat("### READ COMMAND LINE - CONFIGURATION END ###\n\n")
  }
}
