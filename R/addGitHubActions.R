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
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom usethis use_github_action use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".") {
  standardactionfile <- paste0(lib, "/.github/workflows/test-buildlibrary.yaml")
  if (file.exists(standardactionfile)) {
    file.remove(standardactionfile)
  }
  usethis::use_github_action(url = system.file("extdata/test-buildlibrary.yaml", package = "lucode2"),
                             save_as = "test-buildlibrary.yaml")
  if (!file.exists(paste0(lib, "/codecov.yml"))) {
    usethis::use_coverage("codecov")
    file.copy(system.file("extdata/codecov.yml", package = "lucode2"), paste0(lib, "/codecov.yml"), overwrite = TRUE)
  }
  testfolder <- paste0(lib, "/tests/testthat")
  if (!file.exists(testfolder)) {
    dir.create(testfolder, recursive = TRUE)
  }
  if (length(dir(testfolder)) == 0) {
    writeLines('skip("dummy test")', paste0(testfolder, "/test-dummy.R"))
  }
}
