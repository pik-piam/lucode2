#' Read Arguments from command line
#'
#' Function reads arguments from command line of the structure value=content
#' and transforms them to R-Values, if they are called as allowed arguments.
#'
#' @param \dots arguments allowed to be read from command line (other values
#' are ignored). Value is set if found on command line input, nothing is done,
#' if value is not found.
#' @param .argv command line arguments, usually read with commandArgs, can be specified
#' for testing purposes
#' @param .envir environment in which the variables should be written (by
#' default the environment from which the function is called)
#' @param .flags named vector with possible command line switches. Element names
#' are short flags used with one dash, corresponding elements the long form
#' including two dashes: c(t = "--test") will interpret "-t" in command line as "--test"
#' @param .silent boolean which allows to suppress status messages
#' @return vector of activated flags, if any
#' @author Jan Philipp Dietrich, Oliver Richters
#' @export
#' @seealso \code{\link{manipulateConfig}}
#' @examples
#'
#' # Create an R-file "test.R" with following code:
#' value1 <- "old"
#' value2 <- 2
#' value3 <- "willstaythesame"
#' flags <- readArgs("value1", "value2", "value4", .flags = c(t = "--test", p = "--parallel"))
#' message(value1)
#' message(value2)
#' message(value3)
#' if ("--test" %in% flags) {
#'   message("You are in test mode")
#' }
#' if ("--parallel" %in% flags) {
#'   message("You are in parallel mode")
#' }
#'
#' # Open the command line and execute the following code:
#' # Rscript test.R -t --parallel value1=new value2=3 value3=isnotallowed
#'
#' # Output:
#' #
#' # ### READ COMMAND LINE - ASSIGNED CONFIGURATION ###
#' # value1 <- new
#' # value2 <- 3
#' # value4 not defined
#' # Flags: --parallel, --test
#' # ### READ COMMAND LINE - CONFIGURATION END ###
#' #
#' # new
#' # 3
#' # willstaythesame
#' # You are in test mode
#' # You are in parallel mode
#'
#'
#' ### function that reads all allowed arguments from command line ###
readArgs <- function(..., .argv = commandArgs(trailingOnly = TRUE),
                     .envir = parent.frame(), .flags = NULL, .silent = FALSE) {
  allowedArgs <- c(...)
  # search for strings that look like -i1asrR and transform them into long flags
  oneDashFlags <- unlist(strsplit(paste0(.argv[grepl("^-[a-zA-Z0-9]*$", .argv)], collapse = ""), split = ""))
  twoDashFlags <- .argv[grepl("^--[a-zA-Z0-9]*$", .argv) & .argv %in% .flags]
  knownFlags <- sort(unique(c(twoDashFlags, unlist(.flags[names(.flags) %in% oneDashFlags]))))
  unknownFlags <- c(sort(paste0("-", oneDashFlags)[!oneDashFlags %in% c(names(.flags), "-")]),
                    .argv[grepl("^--[a-zA-Z0-9]*$", .argv) & !.argv %in% .flags])
  .argv <- .argv[!grepl("^-", .argv)]
  if (length(.argv) > 0) {
    ### apply additional command line arguments ###
    for (argnr in seq_along(.argv)) {
      for (i in seq_along(allowedArgs)) {
        if (strsplit(.argv[argnr], "=")[[1]][1] == allowedArgs[i]) {
          assign(allowedArgs[i], extract_arguments(.argv[argnr]), envir = .envir)
        }
      }
    }
  }
  if (!.silent) {
    message("\n### READ COMMAND LINE - ASSIGNED CONFIGURATION ###")
    eprint_list(allowedArgs, envir = .envir)
    if (length(knownFlags) > 0) {
      message("Flags: ", paste(knownFlags, collapse = ", "))
    }
    if (length(unknownFlags) > 0) {
      message("Unknown flags: ", paste(unknownFlags, collapse = ", "),
              ". Only available: ", paste0("-", names(.flags), "/", .flags, collapse = ", "))
    }
    message("### READ COMMAND LINE - CONFIGURATION END ###\n")
  }
  if (length(knownFlags) > 0) return(knownFlags)
}
