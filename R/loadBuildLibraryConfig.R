#' buildLibrary
#'
#' Load the build configuration from .buildLibrary. If the file does not exist it is created.
#'
#' @param lib Path to the package
#' @return The configuration loaded from .buildLibrary as a list.
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom yaml read_yaml write_yaml
loadBuildLibraryConfig <- function(lib) {
  if (!file.exists(file.path(lib, ".buildlibrary"))) {
    # if not yet available, add .buildlibrary and add to .Rbuildignore
    cfg <- list(
      ValidationKey = 0,
      AutocreateReadme = TRUE,
      AcceptedWarnings = c(
        "Warning: package '.*' was built under R version",
        "Warning: namespace '.*' is not available and has been replaced"
      ),
      AcceptedNotes = NULL,
      allowLinterWarnings = FALSE
    )
    write_yaml(cfg, file.path(lib, ".buildlibrary"))
    message("Created .buildlibrary config file and added it to .Rbuildignore. Please add it to your next commit!")
    if (file.exists(file.path(lib, ".Rbuildignore"))) {
      a <- c(readLines(file.path(lib, ".Rbuildignore")), "^\\.buildlibrary$")
      if (anyDuplicated(a)) {
        a <- a[!duplicated(a)]
      }
    } else {
      a <- "^\\.buildlibrary$"
    }
    writeLines(a, file.path(lib, ".Rbuildignore"))
  }

  buildLibraryConfigPath <- file.path(lib, ".buildlibrary")

  # remove superfluous UseGithubActions setting from .buildLibrary
  writeLines(grep("UseGithubActions", readLines(buildLibraryConfigPath), value = TRUE, invert = TRUE),
             buildLibraryConfigPath)

  cfg <- read_yaml(buildLibraryConfigPath)

  if (is.null(cfg$AutocreateReadme)) {
    cfg$AutocreateReadme <- TRUE # nolint
  }
  if (is.null(cfg$allowLinterWarnings)) {
    cfg$allowLinterWarnings <- TRUE
  }

  return(cfg)
}
