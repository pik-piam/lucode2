#' check
#'
#' Runs devtools::test(), devtools::check() (without tests), and lucode2::lint()
#'
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param config A configuration defining AcceptedWarnings, AcceptedNotes, and allowLinterWarnings. By default the
#' .buildLibrary file is read.
#'
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @examples
#' \dontrun{
#' lucode2::check()
#' }
#' @importFrom devtools check test
#' @seealso \code{\link{buildLibrary}}, \code{\link{lint}}
#' @seealso \code{\link[devtools]{check}}, \code{\link[devtools]{test}}
#' @export
check <- function(lib = ".", cran = TRUE, config = loadBuildLibraryConfig(lib)) {
  ########### Run checks ###########
  checkResults <- devtools::check(lib, cran = cran, args = "--no-tests")

  # Filter warnings and notes which are accepted
  for (acceptedWarning in config[["AcceptedWarnings"]]) {
    checkResults[["warnings"]] <- grep(acceptedWarning, checkResults[["warnings"]], value = TRUE, invert = TRUE)
  }
  for (acceptedNote in config[["AcceptedNotes"]]) {
    checkResults[["notes"]] <- grep(acceptedNote, checkResults[["notes"]], value = TRUE, invert = TRUE)
  }
  print(checkResults)

  if (length(checkResults[["errors"]]) > 0) {
    stop("The package check showed errors. You need to fix these errors first before submission!")
  }

  if (length(checkResults[["warnings"]]) > 0) {
    stop("The package check showed warnings. You need to take care of these warnings first before submission!")
  }

  if (length(checkResults[["notes"]]) > 0) {
    stop("The package check showed notes. You need to take care of these notes first before submission!")
  }


  ########### Run tests ###########
  testResults <- as.data.frame(devtools::test(lib))

  if (sum(testResults[["failed"]]) > 0 || any(testResults[["error"]])) {
    stop("Some tests failed, please fix them first.")
  }

  unacceptedWarnings <- testResults[["result"]] %>%
    Reduce(f = append, init = list()) %>% # combine results from all tests
    Filter(f = function(result) inherits(result, c("warning", "expectation_warning"))) %>% # keep only warnings
    Filter(f = function(aWarning) { # filter accepted warnings
      return(!any(vapply(config[["AcceptedWarnings"]],
                         function(acceptedWarning) grepl(acceptedWarning, aWarning[["message"]]),
                         logical(1))))
    })
  if (length(unacceptedWarnings) > 0) {
    stop("The package tests produced warnings. Before submission you need to take care of the following warnings:\n",
         unacceptedWarnings %>%
           vapply(function(x) paste0('test "', x[["test"]], '": ', x[["message"]]), character(1)) %>%
           paste(collapse = "\n"))
  }


  ########### Run linter ###########
  linterResult <- lint(getFilesToLint(lib))
  print(linterResult)
  if (length(linterResult) > 0) {
    if (isFALSE(config[["allowLinterWarnings"]])) {
      stop(paste(
        "There were linter warnings. They have to be fixed to successfully complete lucode2::buildLibrary. Running",
        "lucode2::autoFormat() might fix some warnings. If really needed (e.g. to prevent breaking an interface), see",
        "?lintr::exclude on how to disable the linter for some lines."
      ))
    } else {
      warning(paste(
        "There were linter warnings. It is not mandatory to fix them, they do not prevent buildLibrary from finishing",
        "normally. Still, please fix all linter warnings in new code and ideally also some in old code. Running",
        "lucode2::autoFormat() might fix some warnings automatically. If really needed (e.g. to prevent breaking an",
        "interface), see ?lintr::exclude on how to disable the linter for some lines."
      ))
    }
  }
}
