#' checkup
#'
#' Checks the current R setup for common problems and reports info such as OS and R version.
#'
#' @return Invisibly, the report as a list.
#'
#' @author Pascal Führlich
#'
#' @importFrom stats setNames
#' @importFrom tools md5sum
#' @importFrom utils old.packages str tail
#' @export
checkup <- function() {
  report <- R.Version()[c("platform", "version.string")]
  report[["repos"]] <- as.character(getOption("repos"))
  report[["locale"]] <- Sys.getlocale()
  report[["libPaths"]] <- .libPaths() # nolint

  report[["gams"]] <- FALSE
  if (requireNamespace("gdx", quietly = TRUE) && requireNamespace("gdxrrw", quietly = TRUE)) {
    gdx:::.onLoad(NULL, NULL) # initialize gams # nolint
    report[["gams"]] <- gdxrrw::igdx(silent = TRUE, returnStr = TRUE)
  }

  report[["pandoc"]] <- if (requireNamespace("rmarkdown", quietly = TRUE))
    as.character(rmarkdown::pandoc_version())
  else
    "? (Run `install.packages('rmarkdown')` to enable pandoc check.)"

  rProfileUser <- Sys.getenv("R_PROFILE_USER")
  if (rProfileUser == "") {
    if (file.exists("./.Rprofile")) {
      rProfileUser <- normalizePath("./.Rprofile")
    } else {
      rProfileUser <- normalizePath("~/.Rprofile", mustWork = FALSE) # nolint
    }
  }
  if (file.exists(rProfileUser)) {
    report[["Rprofile"]] <- rProfileUser
  } else {
    report[["Rprofile"]] <- "NOT FOUND"
    warning("The Rprofile '", rProfileUser, "' does not exist. The following files exist in that folder:\n",
            paste(list.files(dirname(rProfileUser), all.files = TRUE, no.. = TRUE), collapse = "\n"))
  }

  str(report)

  message("Checking system for known potential problems... ", appendLF = FALSE)

  report[["warnings"]] <- list()

  # Rscript uses the same version of R as this R session
  rscriptVersion <- tail(system("Rscript -e 'cat(R.version.string)'", intern = TRUE, ignore.stderr = TRUE), 1)
  if (rscriptVersion != R.version.string) {
    report[["warnings"]][["rscriptVersion"]] <- paste0("Rscript uses ", rscriptVersion, ", but this session uses ",
                                                       R.version.string)
    warning(report[["warnings"]][["rscriptVersion"]])
  }

  # check if packages are outdated
  outdatedPackages <- old.packages()
  if (length(outdatedPackages) > 0) {
    report[["warnings"]][["outdatedPackages"]] <- list(
      message = paste0("The following packages can be updated:\n",
                       paste("-", outdatedPackages[, "Package"], ":", outdatedPackages[, "Installed"], "->",
                             outdatedPackages[, "ReposVer"],
                             collapse = "\n"),
                       "\nConsider running `update.packages()`."),
      object = outdatedPackages
    )
    warning(report[["warnings"]][["outdatedPackages"]][["message"]])
  }

  # check if a package is installed multiple times in different libPaths
  packagesInLibPaths <- lapply(.libPaths(), list.files) # nolint
  names(packagesInLibPaths) <- .libPaths() # nolint
  duplicatePackages <- findDuplicates(packagesInLibPaths)
  if (length(duplicatePackages) > 0) {
    report[["warnings"]][["duplicatePackages"]] <- list(
      message = paste0("The following packages are installed in multiple libPaths:\n",
                       paste0(names(duplicatePackages), ": ", duplicatePackages, collapse = "\n"),
                       "Consider uninstalling one version via `remove.packages('packageName', lib = 'libPath')`"),
      object = duplicatePackages
    )
    warning(report[["warnings"]][["duplicatePackages"]][["message"]])
  }

  # check if multiple paths in PATH contain programs with the same name but different content
  pathEnvironmentVariable <- unique(strsplit(Sys.getenv("PATH"), ":")[[1]])
  filesInPath <- lapply(pathEnvironmentVariable, list.files)
  names(filesInPath) <- pathEnvironmentVariable
  duplicatesInPath <- findDuplicates(filesInPath)
  stopifnot(length(unique(names(duplicatesInPath))) == length(duplicatesInPath))
  shadowingInPath <- duplicatesInPath[vapply(names(duplicatesInPath), function(programName) {
    return(tryCatch({
      length(unique(md5sum(file.path(duplicatesInPath[[programName]], programName)))) >= 2
    }, warning = function(unused) FALSE, error = function(unused) FALSE))
  }, logical(1))]
  if (length(shadowingInPath) > 0) {
    report[["warnings"]][["shadowingInPath"]] <- list(
      message = paste0("Different versions of the following programs were found in your PATH environment variable:\n",
                       paste0(names(shadowingInPath), ": ", shadowingInPath, collapse = "\n"),
                       "\nConsider renaming/uninstalling all but one version of each program to remove ambiguity."),
      object = shadowingInPath
    )
    warning(report[["warnings"]][["shadowingInPath"]][["message"]])
  }

  # check if at least one of the PIK CRAN repos is used
  expectedRepos <- c("https://pik-piam.r-universe.dev", "https://rse.pik-potsdam.de/r/packages")
  if (length(intersect(report[["repos"]], expectedRepos)) == 0) {
    report[["warnings"]][["expectedRepos"]] <- paste0(
      "No PIK CRAN in getOption('repos'). Consider adding the RSE server and/or r-universe by running\n",
      paste0("options(repos = c(getOption('repos'), '", expectedRepos, "'))", collapse = "\n")
    )
    warning(report[["warnings"]][["expectedRepos"]])
  }

  # check if multiple modules with the same basename are loaded
  module <- try(system2("module", c("list", "-t"), stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (!inherits(module, "try-error")) {
    moduleBasename <- sub("/.*$", "", module)
    duplicateModule <- findDuplicates(as.list(setNames(moduleBasename, module)))
    if (length(duplicateModule) > 0) {
      report[["warnings"]][["moduleDuplicates"]] <- paste0(
        "Multiple modules with the same basename are loaded:\n",
        paste0(names(duplicateModule), ": ", duplicateModule, collapse = "\n")
      )
      warning(report[["warnings"]][["moduleDuplicates"]])
    }
  }

  message("done.")
  return(invisible(report))
}
