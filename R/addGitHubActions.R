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
#' @importFrom usethis local_project use_github_action use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".") {
  local_project(lib, quiet = TRUE)

  if (file.exists("DESCRIPTION") && desc("DESCRIPTION")$get("Package") == "lucode2") {
    question <- "Replace inst/extdata/lucode2-check.yaml with .github/workflows/lucode2-check.yaml?"
    if (md5sum("./.github/workflows/lucode2-check.yaml") != md5sum("./inst/extdata/lucode2-check.yaml") &&
        (!interactive() || !askYesNo(question))) {
      stop("inst/extdata/lucode2-check.yaml != .github/workflows/lucode2-check.yaml")
    }
    file.copy("./.github/workflows/lucode2-check.yaml", "./inst/extdata/", overwrite = TRUE)
  } else {
    actionPath <- system.file("extdata", "lucode2-check.yaml", package = "lucode2")
    githubAction <- sub("autoupdate_schedule: weekly", "autoupdate_schedule: quarterly", readLines(actionPath))
    writeLines(githubAction, "./.github/workflows/lucode2-check.yaml")
  }

  if (!file.exists("codecov.yml")) {
    use_coverage("codecov")
    file.copy(system.file(file.path("extdata", "codecov.yml"), package = "lucode2"), "codecov.yml", overwrite = TRUE)
  }
}
