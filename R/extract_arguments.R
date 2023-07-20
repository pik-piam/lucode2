#' Extract arguments
#'
#'
#' Extracts the value (right-hand-side) of a string of the structure "name=value" and converts it to an appropriate
#' format. This file also reads arguments from command line. To use this script you have to include it by typing
#' source("readArgs.R") in your script and call readArgs(allowed_args) including all arguments that can be read from
#' command line.
#'
#'
#' @param inputArg string of the structure "name=value"
#' @return \item{value }{the value (right-hand-side) of the string converted
#' into an appropriate format}
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readArgs}}
#' @examples
#' \dontrun{
#' extract_arguments("bla=1:9")
#' # [1] 1 2 3 4 5 6 7 8 9
#'
#' extract_arguments("blub=3,5,7")
#' # [1] 3 5 7
#'
#' extract_arguments("ble=hallo")
#' # [1] "hallo"
#' }

# define function for extraction of indicies sets from command line input
extract_arguments <- function(inputArg) {  # nolint
  if (length(grep("=", inputArg) > 0)) inputArg <- sub(".*?=", "", inputArg) # everything after first =
  if (inputArg == "") return("")
  if (length(grep(",", inputArg) > 0)) {
    return(unlist(sapply(strsplit(inputArg, ",")[[1]], extract_arguments), use.names = FALSE)) # nolint
  }
  if (length(strsplit(inputArg, ":")[[1]]) == 2) {
    if (suppressWarnings(is.na(as.integer(strsplit(inputArg, ":")[[1]][1])) |
                           is.na(as.integer(strsplit(inputArg, ":")[[1]][2])))) {
      extractedArguments <- inputArg
    } else {
      extractedArguments <- as.integer(strsplit(inputArg, ":")[[1]][1]):as.integer(strsplit(inputArg, ":")[[1]][2])
    }
  } else {
    if (sum(suppressWarnings(is.na(as.numeric(strsplit(inputArg, ",")[[1]]))) == 0)) {
      extractedArguments <- as.numeric(strsplit(inputArg, ",")[[1]])
    } else {
      if (suppressWarnings(is.na(as.logical(inputArg)))) {
        extractedArguments <- strsplit(inputArg, ",")[[1]]
        if (extractedArguments == "NULL") extractedArguments <- NULL
      } else {
        extractedArguments <- as.logical(inputArg)
      }
    }
  }
  return(extractedArguments)
}
