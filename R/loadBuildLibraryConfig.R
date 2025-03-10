#' buildLibrary
#'
#' Load the build configuration from .buildLibrary. If the file does not exist it is created.
#'
#' @param lib Path to the package
#' @return The configuration loaded from .buildLibrary as a list.
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @seealso \code{\link{buildLibrary}}
#' @importFrom utils askYesNo
#' @importFrom yaml read_yaml write_yaml
loadBuildLibraryConfig <- function(lib = ".") {
  lib <- normalizePath(lib)
  if (!file.exists(file.path(lib, ".buildlibrary"))) {
    if (interactive()) {
      message("Config file .buildlibrary not found in '", lib, "'. Possible reason: '", lib, "' ",
              if (lib == normalizePath(".")) "(= your working directory) " else "",
              "is not the main folder of a package.")

      if (!isTRUE(askYesNo(paste0("Do you want to create .buildLibrary in '", lib, "'?"), default = FALSE))) {
        stop(".buildlibrary was not created, cannot continue.")
      }
    }

    # if not yet available, add .buildlibrary and add to .Rbuildignore
    cfg <- list(ValidationKey = 0,
                AutocreateReadme = TRUE,
                AutocreateCITATION = TRUE,
                AcceptedWarnings = c("Warning: package '.*' was built under R version",
                                     "Warning: namespace '.*' is not available and has been replaced"),
                AcceptedNotes = NULL,
                allowLinterWarnings = FALSE)
    write_yaml(cfg, file.path(lib, ".buildlibrary"))
    message("Created .buildlibrary config file. Please add it to your next commit!")
  }

  rbuildIgnore <- c(
    if (file.exists(file.path(lib, ".Rbuildignore"))) readLines(file.path(lib, ".Rbuildignore")) else NULL,
    "^\\.buildlibrary$",
    "^\\.pre-commit-config\\.yaml$",
    "^Makefile$",
    "^workflow$"
  )
  rbuildIgnore <- rbuildIgnore[!duplicated(rbuildIgnore)]
  writeLines(rbuildIgnore, file.path(lib, ".Rbuildignore"))

  buildLibraryConfigPath <- file.path(lib, ".buildlibrary")

  # remove superfluous UseGithubActions setting from .buildLibrary
  writeLines(grep("UseGithubActions", readLines(buildLibraryConfigPath), value = TRUE, invert = TRUE),
             buildLibraryConfigPath)

  cfg <- read_yaml(buildLibraryConfigPath)

  if (is.null(cfg$AutocreateReadme)) {
    cfg$AutocreateReadme <- TRUE # nolint
  }

  if (is.null(cfg$AutocreateCITATION)) {
    cfg$AutocreateCITATION <- TRUE
  }

  if (is.null(cfg$allowLinterWarnings)) {
    cfg$allowLinterWarnings <- TRUE
  }

  if (is.null(cfg$enforceVersionUpdate)) {
    cfg$enforceVersionUpdate <- FALSE
  }

  if (is.null(cfg$skipCoverage)) {
    cfg$skipCoverage <- FALSE
  }

  return(cfg)
}
