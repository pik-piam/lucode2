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
#' @author Jan Philipp Dietrich, Pascal Sauer
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
  if (!file.exists(packageDocumentation) && !file.exists(file.path("R", paste0(packageName, ".R")))) {
    writeLines(c("# The package documentation is defined in this file.",
                 "# You can get it via `library(<package>); ?<package>`.",
                 "#' @docType package",
                 '"_PACKAGE"'), packageDocumentation)
  }

  document(pkg = ".", roclets = c("rd", "collate", "namespace", "vignette"))

  logs <- list()
  processes <- list()

  ########### Run linter ###########
  if (runLinter) {
    logs[["linter"]] <- withr::local_tempfile(pattern = "linter", fileext = ".log")
    message("running linter in background, see ", logs[["linter"]])
    processes[["linter"]] <- callr::r_bg(function(...) lucode2::verifyLinter(...),
                                         args = list(isFALSE(config[["allowLinterWarnings"]])),
                                         stdout = logs[["linter"]], stderr = "2>&1")
  }

  ########### Run tests ###########
  logs[["test"]] <- withr::local_tempfile(pattern = "test", fileext = ".log")
  message("running tests in background, see ", logs[["test"]])
  # run tests in separate R session for parallelization and so that test results are independent of
  # anything set in the current R session
  processes[["test"]] <- callr::r_bg(function(...) lucode2::verifyTests(...),
                                     args = list(config[["AcceptedWarnings"]]),
                                     stdout = logs[["test"]], stderr = "2>&1")

  ########### Run checks ###########
  processes[["check"]] <- callr::r_bg(function(...) lucode2::verifyCheck(...),
                                      args = list(cran, config[["AcceptedWarnings"]], config[["AcceptedNotes"]]),
                                      stdout = "", stderr = "2>&1") # will print to main process' terminal

  ########### Wait for processes ###########
  while (length(processes) >= 1) {
    Sys.sleep(2)
    finishedProcesses <- Filter(processes, f = function(p) !p$is_alive())
    for (processId in names(finishedProcesses)) {
      throwError <- tryCatch({
        processes[[processId]]$get_result()
        FALSE
      }, error = function(error) {
        for (p in processes) {
          p$kill_tree()
        }
        if (processId %in% names(logs)) {
          message("\n\naborting due to ", processId, " error:")
          message(paste(readLines(logs[[processId]]), collapse = "\n"))
        }
        return(TRUE)
      })
      if (throwError) {
        stop("lucode2::check failed")
      }
    }
    processes <- processes[!names(processes) %in% names(finishedProcesses)]
  }

  for (logId in names(logs)) {
    message("\n\n", logId, " log:")
    message(paste(readLines(logs[[logId]], warn = FALSE), collapse = "\n"))
  }

  return(invisible(NULL))
}


#' verifyTests
#'
#' Run tests and stop on error or unaccepted warning.
#'
#' @param acceptedWarnings A character vector of regular expressions.
#' A warning will result in an error unless it matches one of these regular expressions.
#'
#' @author Pascal Sauer
#' @export
verifyTests <- function(acceptedWarnings) {
  testResults <- devtools::test(stop_on_failure = TRUE, reporter = "summary") %>%
    Reduce(f = function(a, b) append(a, b[["results"]]), init = list()) # combine results from all tests

  # stop if there was at least one unaccepted warning
  unacceptedWarnings <- testResults %>%
    Filter(f = function(result) inherits(result, c("warning", "expectation_warning"))) %>% # keep only warnings
    Filter(f = function(aWarning) { # filter accepted warnings
      return(!any(vapply(acceptedWarnings,
                         function(acceptedWarning) grepl(acceptedWarning, aWarning[["message"]]),
                         logical(1))))
    })
  if (length(unacceptedWarnings) > 0) {
    message("Before submission you need to take care of the following warnings:\n")
    unacceptedWarnings %>%
      vapply(function(x) paste0('test "', x[["test"]], '": ', x[["message"]]), character(1)) %>%
      paste(collapse = "\n") %>%
      message()
    message("\nYou can find solutions to common problems at ",
            "https://github.com/pik-piam/discussions/discussions/18")
    stop("The package tests produced warnings.")
  }
}

#' verifyLinter
#'
#' Run linter and stop on linter warning unless linter warnings are allowed.
#'
#' @param allowLinterWarnings If FALSE (the default) will stop on linter warnings.
#'
#' @author Pascal Sauer
#' @export
verifyLinter <- function(allowLinterWarnings = FALSE) {
  linterResults <- lucode2::lint()
  message("linter results:")
  print(linterResults)
  if (length(linterResults) > 0) {
    autoFormatExcludeInfo <- paste("Running lucode2::autoFormat() might fix some warnings. If really needed (e.g.",
                                   "to prevent breaking an interface), see ?lintr::exclude on how to disable the",
                                   "linter for some lines.")
    if (allowLinterWarnings) {
      stop("There were linter warnings. They have to be fixed to successfully complete lucode2::buildLibrary. ",
           autoFormatExcludeInfo,
           " You can find solutions to common problems at https://github.com/pik-piam/discussions/discussions/18")
    } else {
      warning("There were linter warnings. It is not mandatory to fix them, they do not prevent buildLibrary ",
              "from finishing normally. Still, please fix all linter warnings in new code and ideally also ",
              "some in old code. ", autoFormatExcludeInfo)
    }
  } else {
    message("No linter warnings - great :D")
  }
}

#' verifyCheck
#'
#' Run R CMD check completely without stopping. Then stop on errors, or unaccepted warnings and notes.
#'
#' @param cran Passed to devtools::check
#' @param acceptedWarnings A character vector of regular expressions.
#' A warning will result in an error unless it matches one of these regular expressions.
#' @param acceptedNotes A character vector of regular expressions.
#' A note will result in an error unless it matches one of these regular expressions.
#'
#' @author Pascal Sauer
#' @export
verifyCheck <- function(cran, acceptedWarnings, acceptedNotes) {
  withr::local_options(crayon.enabled = TRUE)
  # _R_CHECK_SYSTEM_CLOCK_ = 0 should prevent "unable to verify current time" when time server is down
  checkResults <- devtools::check(document = FALSE, cran = cran, args = c("--timings", "--no-tests"),
                                  env_vars = c(NOT_CRAN = "true", `_R_CHECK_SYSTEM_CLOCK_` = "0"),
                                  error_on = "never")

  # Filter warnings and notes which are accepted
  for (acceptedWarning in acceptedWarnings) {
    checkResults[["warnings"]] <- grep(acceptedWarning, checkResults[["warnings"]], value = TRUE, invert = TRUE)
  }
  for (acceptedNote in acceptedNotes) {
    checkResults[["notes"]] <- grep(acceptedNote, checkResults[["notes"]], value = TRUE, invert = TRUE)
  }

  for (type in c("errors", "warnings", "notes")) {
    if (length(checkResults[[type]]) > 0) {
      message("\nThe following errors/warnings/notes are mandatory to fix:\n")
      print(checkResults)
      message("\nYou can find solutions to common problems at ",
              "https://github.com/pik-piam/discussions/discussions/18")
      stop("There were errors/warnings/notes that are mandatory to fix during R CMD check.")
    }
  }
}
