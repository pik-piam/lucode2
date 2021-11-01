#' addGitHubActions
#'
#' This function adds a standard Github action workflow called "lucode2-check.yaml" to the project which runs
#' lucode2::check(), checks the validation key, and creates a coverage report using codecov. This file is overwritten
#' automatically each time this function is run and should not be edited by hand. Additional Github actions can be
#' added as separate files.
#'
#' In addition, this function adds a codecov.yml to the repository, if not already existing. This file
#' is only created if missing and can be edited manually.
#'
#' @param lib Path to the package
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom desc desc
#' @importFrom usethis use_github_action use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".") {
  unlink(file.path(lib, ".github", "workflows", "test-buildlibrary.yaml")) # the old workflow, remove this at some point

  # do not overwrite workflow file in lucode2, otherwise a workflow file change would be overwritten by buildLibrary
  if (!file.exists(file.path(lib, "DESCRIPTION")) ||
      desc(file = file.path(lib, "DESCRIPTION"))[["get"]]("Package") != "lucode2") {
    unlink(file.path(lib, ".github", "workflows", "lucode2-check.yaml"))
    use_github_action(
      url = "https://raw.githubusercontent.com/pik-piam/lucode2/master/.github/workflows/lucode2-check.yaml")
  }

  if (!file.exists(file.path(lib, "codecov.yml"))) {
    use_coverage("codecov")
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
}
