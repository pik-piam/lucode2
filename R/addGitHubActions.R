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
#' @importFrom usethis proj_set use_github_action use_coverage
#' @importFrom withr local_dir
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".") {
  local_dir(lib)

  # remove old workflow file, remove this line at some point
  unlink(file.path(".github", "workflows", "test-buildlibrary.yaml"))

  # do not overwrite workflow file in lucode2, otherwise a workflow file change would be overwritten by buildLibrary
  if (!file.exists("DESCRIPTION") || desc("DESCRIPTION")$get("Package") != "lucode2") {
    unlink(file.path(".github", "workflows", "lucode2-check.yaml"))
    use_github_action(NULL, # name is not used when a url is passed
                      "https://raw.githubusercontent.com/pik-piam/lucode2/master/.github/workflows/lucode2-check.yaml")
  }

  if (!file.exists("codecov.yml")) {
    use_coverage("codecov")
    file.copy(system.file(file.path("extdata", "codecov.yml"), package = "lucode2"), "codecov.yml", overwrite = TRUE)
  }
}
