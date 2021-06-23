#' addGitHubActions
#'
#' adds GitHubActions with a standard workflow including codecov to the package. It will add a standard
#' Github action "test-buildlibrary.yml" to the project which performs standard tests as well as
#' creates a coverage report. This file is overwritten automatically each time this function is run and
#' should not be edited by hand. But additional Github actions can be added as separate files.
#'
#' In addition, this function adds a codecov.yml to the repository, if not already existing. This file
#' is only created, if missing and can be edited manually.
#'
#' @param lib Path to the package
#' @param removeTravis If TRUE try to remove existing travis configuration and files.
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom usethis use_github_action use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".", removeTravis = TRUE) {
  standardactionfile <- file.path(lib, ".github", "workflows", "test-buildlibrary.yaml")
  if (file.exists(standardactionfile)) {
    file.remove(standardactionfile)
  }
  usethis::use_github_action(
    url = system.file(file.path("extdata", "test-buildlibrary.yaml"), package = "lucode2"),
    save_as = "test-buildlibrary.yaml"
  )
  if (!file.exists(file.path(lib, "codecov.yml"))) {
    usethis::use_coverage("codecov")
    file.copy(system.file(file.path("extdata", "codecov.yml"), package = "lucode2"),
              file.path(lib, "codecov.yml"),
              overwrite = TRUE)
  }
  testfolder <- file.path(lib, "tests", "testthat")
  if (!file.exists(testfolder)) {
    dir.create(testfolder, recursive = TRUE)
  }
  if (length(dir(testfolder)) == 0) {
    writeLines('skip("dummy test")', file.path(testfolder, "test-dummy.R"))
  }

  if (removeTravis) {
    # remove travis related parts
    travisfile <- file.path(lib, ".travis.yml")
    if (file.exists(travisfile)) {
      file.remove(travisfile)
      rbuildignore <- file.path(lib, ".Rbuildignore")
      if (file.exists(rbuildignore)) {
        rbuildignoreContent <- readLines(rbuildignore)
        writeLines(grep("travis", rbuildignoreContent, value = TRUE, invert = TRUE), rbuildignore)
      }
      testfolder <- file.path(lib, "tests", "testthat")
      if (!file.exists(testfolder)) {
        dir.create(testfolder, recursive = TRUE)
      }
      travistest <- file.path(lib, "tests", "testthat", "test-travisCI.R")
      if (file.exists(travistest)) {
        file.remove(travistest)
      }
      if (length(dir(testfolder) == 0)) {
        writeLines('skip("dummy test")', file.path(testfolder, "test-dummy.R"))
      }
    }
  }
}
