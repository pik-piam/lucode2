#' check
#'
#' Builds documentation and runs checks, tests, and linter. Find solutions to common problems at
#' https://github.com/pik-piam/discussions/discussions/18
#'
#' This function builds documentation including vignettes via devtools::document(). It runs devtools::check()
#' (without tests), then in a separate clean R session it runs devtools::test(), and finally lucode2::lint().
#' Before linting ".lintr" config files are created if missing. The actual linter rules are defined
#' in \code{\link{lintrRules}}. In general undesirable functions and operators
#' result in linter warnings, but not in the tests and vignettes subdirectories. Warnings and notes in checks
#' and tests are only allowed if the given config defines them as accepted, otherwise this function will stop.
#'
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param config A configuration defining AcceptedWarnings, AcceptedNotes, and allowLinterWarnings. By default the
#' .buildLibrary file is read.
#' @param runLinter Set to FALSE to skip the linter.
#'
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @examples
#' \dontrun{
#' lucode2::check()
#' }
#' @importFrom callr r
#' @importFrom devtools document check
#' @importFrom withr local_tempdir
#' @seealso \code{\link{buildLibrary}}, \code{\link{lint}}, \code{\link{lintrRules}}
#' @seealso \code{\link[devtools]{check}}, \code{\link[devtools]{test}}
#' @export
check <- function(lib = ".", cran = TRUE, config = loadBuildLibraryConfig(lib), runLinter = TRUE) {
  local_dir(lib)

  packageName <- desc("DESCRIPTION")$get("Package")
  packageDocumentation <- file.path("R", paste0(packageName, "-package.R"))
  if (!file.exists(packageDocumentation)) {
    writeLines(c("# The package documentation is defined in this file.",
                 "# You can get it via `library(<package>); ?<package>`.",
                 "#' @docType package",
                 '"_PACKAGE"'), packageDocumentation)
  }

  document(pkg = ".", roclets = c("rd", "collate", "namespace", "vignette"))

  ########### Run tests ###########
  # run tests in a separate R session so test results are independent of anything set in the current R session
  testResults <- callr::r(function() {
    withr::local_options(crayon.enabled = TRUE)
    return(devtools::test(stop_on_failure = TRUE))
  }, show = TRUE)

  helpLink <- "You can find solutions to common problems at https://github.com/pik-piam/discussions/discussions/18"
  unacceptedWarnings <- testResults %>%
    Reduce(f = function(a, b) append(a, b[["results"]]), init = list()) %>% # combine results from all tests
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
           paste(collapse = "\n"),
         "\n", helpLink)
  }

  ########### Run linter ###########
  if (runLinter) {
    # run linter and check results
    linterResult <- lint()
    print(linterResult)
    if (length(linterResult) > 0) {
      autoFormatExcludeInfo <- paste("Running lucode2::autoFormat() might fix some warnings. If really needed (e.g.",
                                     "to prevent breaking an interface), see ?lintr::exclude on how to disable the",
                                     "linter for some lines.")
      if (isFALSE(config[["allowLinterWarnings"]])) {
        stop("There were linter warnings. They have to be fixed to successfully complete lucode2::buildLibrary. ",
             autoFormatExcludeInfo, " ", helpLink)
      } else {
        warning("There were linter warnings. It is not mandatory to fix them, they do not prevent buildLibrary ",
                "from finishing normally. Still, please fix all linter warnings in new code and ideally also ",
                "some in old code. ", autoFormatExcludeInfo)
      }
    } else {
      message("No linter warnings - great :D")
    }
  }

  ########### Run checks ###########
  checkResults <- devtools::check(document = FALSE, cran = cran, args = "--no-tests", error_on = "never")

  # Filter warnings and notes which are accepted
  for (acceptedWarning in config[["AcceptedWarnings"]]) {
    checkResults[["warnings"]] <- grep(acceptedWarning, checkResults[["warnings"]], value = TRUE, invert = TRUE)
  }
  for (acceptedNote in config[["AcceptedNotes"]]) {
    checkResults[["notes"]] <- grep(acceptedNote, checkResults[["notes"]], value = TRUE, invert = TRUE)
  }
  print(checkResults)

  for (type in c("errors", "warnings", "notes")) {
    if (length(checkResults[[type]]) > 0) {
      stop("The package check showed ", type, ". You need to take care of these ", type,
           " before submission! ", helpLink)
    }
  }
}
