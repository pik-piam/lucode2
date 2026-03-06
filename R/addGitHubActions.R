#' addGitHubActions
#'
#' This function adds a standard Github action workflow called "check.yaml" to the project which runs
#' lucode2::check(), checks the validation key, and creates a coverage report using codecov. This file is overwritten
#' automatically each time this function is run and should not be edited by hand.
#'
#' #' If the config option `UsePkgDown` is set to TRUE in the `.buildlibrary` file,
#' this function also adds a pkgdown.yaml workflow that builds and deploys package documentation
#' to GitHub Pages, and creates a default `_pkgdown.yml` configuration file if it doesn't exist.
#'
#' In addition, this function adds a codecov.yml to the repository, if not already existing. This file
#' is only created if missing and can be edited manually.
#'
#'
#' @param lib Path to the package
#' @param config The build configuration loaded from .buildlibrary. If NULL, loads it automatically.
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom usethis local_project use_coverage
#' @examples
#' \dontrun{
#' addGitHubActions()
#' }
#' @export
addGitHubActions <- function(lib = ".", config = NULL) {
  local_project(lib, quiet = TRUE)

  if (is.null(config)) {
    config <- loadBuildLibraryConfig(lib)
  }

  # remove legacy workflow file (no longer necessary in January 2024)
  unlink(".github/workflows/lucode2-check.yaml")

  conditionalCopy(".github/workflows/check.yaml")

  if (isTRUE(config$UsePkgDown)) {
    conditionalCopy(".github/workflows/pkgdown.yaml")
    if (!file.exists("_pkgdown.yml")) {
      conditionalCopy("_pkgdown.yml")
    }
  }

  if (!file.exists("codecov.yml")) {
    use_coverage("codecov")
    conditionalCopy("codecov.yml")
  }

  if (!"^\\.github$" %in% readLines(".Rbuildignore")) {
    write("^\\.github$", ".Rbuildignore", append = TRUE)
  }
  if (!"^_pkgdown\\.yml$" %in% readLines(".Rbuildignore")) {
    write("^_pkgdown\\.yml$", ".Rbuildignore", append = TRUE)
  }

}
