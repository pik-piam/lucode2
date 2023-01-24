#' addGitHubActions
#'
#' This function adds a standard Github action workflow called "check.yaml" to the project which runs
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
#' @importFrom usethis local_project use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".") {
  local_project(lib, quiet = TRUE)

  # remove legacy workflow file (no longer necessary in January 2024)
  unlink(".github/workflows/lucode2-check.yaml")

  conditionalCopy(".github/workflows/check.yaml")

  if (!file.exists("codecov.yml")) {
    use_coverage("codecov")
    conditionalCopy("codecov.yml")
  }

  if (!"^\\.github$" %in% readLines(".Rbuildignore")) {
    write("^\\.github$", ".Rbuildignore", append = TRUE)
  }

}
