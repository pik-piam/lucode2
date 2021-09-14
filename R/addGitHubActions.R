#' addGitHubActions
#'
#' adds GitHubActions with a standard workflow including codecov to the package. It will add a standard
#' Github action "test-buildlibrary.yml" to the project which performs standard tests as well as
#' creates a coverage report. This file is overwritten automatically each time this function is run and
#' should not be edited by hand. Additional Github actions can be added as separate files.
#'
#' In addition, this function adds a codecov.yml to the repository, if not already existing. This file
#' is only created if missing and can be edited manually.
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
  unlink(file.path(lib, ".github", "workflows", "test-buildlibrary.yaml"))
  use_github_action("test-buildlibrary.yaml",
                    system.file(file.path("extdata", "test-buildlibrary.yaml"), package = "lucode2"))

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
