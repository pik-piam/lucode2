#' buildLibrary
#'
#' Builds R libraries. Includes checks for consistency. Find solutions to common problems at
#' https://github.com/pik-piam/discussions/discussions/18
#'
#' This function is designed to help building and checking R libraries. It performs the
#' following steps: \itemize{
#' \item Version: Determination of a new version number (Can also be defined by the user).
#' \item Date: Determination of a the date of the build (Can also be defined by the user).
#' \item Linter: Check for code style problems.
#' \item R check: Check whether the library is consistent and can be built.
#' \item Package building: Builds the .zip and .tar.gz packages under windows.
#' Under linux, only the .tar.gz package is built. }
#'
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param updateLucode2 Update lucode2 if possible and run buildLibrary with new version instead.
#' @param autoCheckRepoUpToDate Automatically check if your repository is up to date. If FALSE the user is asked.
#' @param updateType Either an integer or character string:
#'
#'   | **number**  | **string**    | **description**                          |
#'   |-------------|---------------|------------------------------------------|
#'   | 1           | `major`       | for API breaking changes                 |
#'   | 2 (default) | `minor`       | for new features or improvements         |
#'   | 3           | `patch`       | for bugfixes and corrections             |
#'   | 4           | `development` | only for packages in development stage   |
#'   | 0           | `none`        | version has already been incremented     |
#'
#'
#' @md
#' @note The behavior of buildLibrary can be configured via the `.buildLibrary` file in the
#' main folder of the package. It uses YAML format and can contain the following entries:
#'
#' * **ValidationKey**: This entry always exists and is written automatically by `buildLibrary`
#'                      It confirms that the package has been successfully build via the function.
#' * **AutocreateReadme** (optional): yes/no - decides whether `buildLibrary` automatically updates
#'                                    the README.md file or not (default: yes)
#' * **AddInReadme** (optional): Additional entries to be added to the autocreated README. Provided either
#'                               in markdown format or as paths to RMarkdown (Rmd) or Markdown (md) files
#' * **AddLogoReadme** (optional): Additional logo to be added to the autocreated README. Provided as
#'                                 path to logo in PNG format
#' * **LogoHeightReadme** (optional): Height of the logo in README in px
#' * **AcceptedWarnings** (optional): a list of Warnings which should be ignored by `buildLibrary`
#'                                    (autocompletion via asterisks allowed)
#' * **AcceptedNotes** (optional): a list of Notes which should be ignored by `buildLibrary`
#'                                    (autocompletion via asterisks allowed)
#' * **allowLinterWarnings**: yes/no - If set to "no" linter warnings will stop the build process.
#'                            (default: yes)
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal Sauer
#' @seealso \code{\link{package2readme}}, \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom desc desc desc_get_deps
#' @importFrom utils old.packages update.packages packageVersion download.file
#' @importFrom withr defer local_connection local_dir
#' @importFrom yaml write_yaml
#' @examples
#' \dontrun{
#' buildLibrary()
#' }
#' @export
buildLibrary <- function(lib = ".", cran = TRUE, updateType = NULL,
                         updateLucode2 = TRUE, autoCheckRepoUpToDate = TRUE) {
  if (isTRUE(updateType == "")) updateType <- NULL
  lib <- normalizePath(lib)
  local_dir(lib)
  checkRepoUpToDate(".", autoCheckRepoUpToDate)

  loadedLucode2Version <- .__NAMESPACE__.$spec[["version"]]

  if (updateLucode2 && !is.null(old.packages(instPkgs = installed.packages()["lucode2", , drop = FALSE]))) {
    message("installing new lucode2 update")
    suppressWarnings({ # prevent Warning: package 'cluster' in library '...' will not be updated
      update.packages(oldPkgs = "lucode2", ask = FALSE)
    })
    stopifnot(`lucode2 update failed` = loadedLucode2Version != packageVersion("lucode2"))
    message("update done.\n")
  }

  # the loaded lucode2 version is different from the version available on disk, usually right after updating lucode2
  if (loadedLucode2Version != packageVersion("lucode2")) {
    # asking questions in RStudio/Jupyter in a callr session does not work, so need updateType to be set
    if (is.null(updateType) && (Sys.getenv("RSTUDIO") == "1" || !is.na(Sys.getenv("JPY_SESSION_NAME", NA)))) {
      warning("Cannot automatically rerun buildLibrary with new lucode2 version in RStudio, ",
              "unless the `updateType` argument is set.")
      stop("An outdated lucode2 version is loaded. Restart the R session (Ctrl+Shift+F10) and try again:\n",
           "lucode2::buildLibrary()")
    }

    message("Running buildLibrary with new lucode2 version in separate R session.\n",
            "In case of problems restart R session.")

    # getLine and similar functions don't work everywhere via callr, so must ask questions before this
    return(invisible(callr::r(function(...) lucode2::buildLibrary(...),
                              args = list(lib, cran, updateType, updateLucode2 = FALSE,
                                          autoCheckRepoUpToDate = NULL),
                              show = TRUE, spinner = FALSE, stdin = "")))
  }

  fixBuildLibraryMergeConflict()
  modifyRproj()

  stopifnot(`No DESCRIPTION file found` = file.exists("DESCRIPTION"))
  packageName <- desc("DESCRIPTION")$get("Package")

  ############################################################
  # load/create .buildLibrary file
  ############################################################
  cfg <- loadBuildLibraryConfig()

  ############################################################
  # import Depends packages
  ############################################################
  dependsPackages <- setdiff(desc_get_deps()[desc_get_deps()["type"] == "Depends", "package"], "R")
  if (length(dependsPackages) > 0) {
    # All packages defined as "Depends" in DESCRIPTION must be imported, otherwise a check fails.
    writeLines(c("# Generated by lucode2: do not edit by hand\n",
                 paste("#' @import", paste(dependsPackages, collapse = " ")),
                 "NULL"),
               file.path("R", "imports.R"))
  }

  ####################################################################
  # Run checks, tests and linter
  ###################################################################
  testfolder <- file.path("tests", "testthat")
  dir.create(testfolder, recursive = TRUE, showWarnings = !file.exists(testfolder))
  if (length(dir(testfolder)) == 0) {
    writeLines('skip("dummy test")', file.path(testfolder, "test-dummy.R"))
  }
  check(cran = cran, config = cfg)

  ####################################################################
  # Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$", list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)
  unlink(rcheckfolders, recursive = TRUE)

  ############################################################
  # add GitHub actions, pre-commit-config, and Makefile
  ############################################################
  tryCatch(addGitHubActions(), error = function(error) {
    message("Could not add GitHub Actions:", error)
  })

  # hidden files in inst/extdata produce NOTE during check, so remove leading dot from .pre-commit-config.yaml
  conditionalCopy(".pre-commit-config.yaml", "pre-commit-config.yaml")
  if (packageName != "lucode2") {
    preCommitConfig <- sub("autoupdate_schedule: weekly", "autoupdate_schedule: quarterly",
                           readLines(".pre-commit-config.yaml"))
    writeLines(preCommitConfig, ".pre-commit-config.yaml")
  }

  conditionalCopy("Makefile")

  ##########################################################
  # Check for version numbers
  ##########################################################
  # Version number in DESCRIPTION
  descfile <- readLines("DESCRIPTION")
  descfileVersion <- sub(pattern = "[^(0-9)]*$", replacement = "", perl = TRUE,
                         x = sub(pattern = "Version:[^(0-9)]*", replacement = "", perl = TRUE,
                                 x = grep(pattern = "Version", x = descfile, value = TRUE)))

  updateType <- handleUpdateType(updateType)
  version <- incrementVersion(descfileVersion, updateType)

  # Change the version in DESCRIPTION
  descfile[grep("Version", descfile)] <- sub(descfileVersion, version, descfile[grep("Version", descfile)])

  ############################################################
  # Check for the date
  ############################################################
  dateToday <- Sys.Date()
  if (any(grepl("Date:", descfile))) {
    descfile[grep("Date:", descfile)] <- paste("Date:", dateToday)
  } else {
    descfile <- append(descfile, paste("Date:", dateToday))
  }

  ############################################################
  # Update validation key
  ############################################################
  cfg[["ValidationKey"]] <- as.character(if (cran) validationkey(version, dateToday) else 0)

  descfile <- descfile[!grepl("ValidationKey:", descfile)]

  ##################################################################
  # Write the modified description files, update metadata and readme
  ##################################################################
  writeLines(descfile, "DESCRIPTION")
  write_yaml(cfg, ".buildlibrary")
  citation::r2cff(export = TRUE)
  if (isTRUE(cfg$AutocreateReadme)) {
    package2readme(add = cfg$AddInReadme,
                   logo = cfg$AddLogoReadme,
                   logoHeight = cfg$LogoHeightReadme)
  }

  ####################################################################
  # remove existing .zenodo.json files as CITATION.cff is now
  # being used instead.
  ###################################################################
  if (file.exists(".zenodo.json")) {
    message("Deleted .zenodo.json as it has been replaced by CITATION.cff")
    file.remove(".zenodo.json")
  }


  ####################################################################
  # Make sure man/*.Rd files are not ignored
  ###################################################################
  unignoreManFiles()

  ############################################################
  # Verbosity for version information and git commands
  ############################################################

  if (updateType != 0) {
    message("Updated from version ", descfileVersion, " to version ", version)
    availablePackages <- available.packages()
    if (packageName %in% dimnames(availablePackages)[[1]]) {
      repoVersion <- package_version(availablePackages[packageName, "Version"])
      if (repoVersion >= version) {
        warning(packageName, " ", repoVersion,
                " is already available online, you might want to update to a higher version number.")
      }
    }
  }
  message("Don't forget to commit and push your changes.\ndone")
}

handleUpdateType <- function(updateType = NULL, title = "Please choose an update type") {
  if (!is.null(updateType)) {
    # convert character updateType parameters to numbers
    updateType <- switch(as.character(updateType),
                         "major" = 1,
                         "1" = 1,
                         "minor" = 2,
                         "2" = 2,
                         "patch" = 3,
                         "3" = 3,
                         "development" = 4,
                         "4" = 4,
                         "none" = 0,
                         "0" = 0,
                         handleUpdateType(title = title))
    return(updateType)
  }
  updateType <- c("major revision (for API breaking changes)",
                  "minor revision (for new features or improvements)",
                  "patch (for bugfixes and corrections)",
                  "only for packages in development stage",
                  "no version increment (only to use if version is already incremented!)")
  cat(title, ":\n", sep = "")
  updateTypeNumber <- c(1:(length(updateType) - 1), 0)
  cat(paste(updateTypeNumber, updateType, sep = ": "), sep = "\n")
  cat("\nNumber: ")
  identifier <- getLine()
  if (any(!(as.numeric(identifier) %in% updateTypeNumber))) {
    message("This choice (", identifier, ") is not possible.")
    identifier <- handleUpdateType(title = paste0("Please type in a number between 0 and ", length(updateType) - 1))
  }
  return(as.numeric(identifier))
}

unignoreManFiles <- function() {
  gitignore <- suppressWarnings(tryCatch({
    readLines(".gitignore")
  }, error = function(error) return(character(0))))
  if ("*.Rd" %in% gitignore || file.exists(file.path("man", ".gitignore"))) {
    message("*.Rd files are currently ignored, but they should be commited.")

    if ("*.Rd" %in% gitignore) {
      message('removing "*.Rd" from .gitignore')
      writeLines(gitignore[gitignore != "*.Rd" &
                             gitignore != "# Help files (because they will be created automatically by roxygen)"],
                 ".gitignore")
    }

    if (file.exists(file.path("man", ".gitignore"))) {
      message("removing man/.gitignore")
      file.remove(file.path("man", ".gitignore"))
    }
  }
}
