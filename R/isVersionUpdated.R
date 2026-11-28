#' isVersionUpdated
#'
#' Checks if the version number in the DESCRIPTION file of a given package has
#' been updated (may not be a version number for development stage packages).
#' The version is compared with the latest version on the package repository and,
#' if \code{base} is given, with the version in the DESCRIPTION file of that git ref.
#' @param repo package repository to determine latest version
#' @param config A configuration defining enforceVersionUpdate. By default
#' the .buildLibrary file is read.
#' @param base A git ref (e.g. "origin/main") to compare the version with, typically
#' the base branch of a pull request. The local version must be strictly greater than
#' the version in that ref's DESCRIPTION file. If NULL (default) no such comparison
#' is done.
#' @importFrom desc desc
#' @importFrom utils packageVersion
#' @author Falk Benke
#' @export
isVersionUpdated <- function(repo = "https://rse.pik-potsdam.de/r/packages/",
                             config = loadBuildLibraryConfig(), base = NULL) {
  if (!file.exists("DESCRIPTION")) stop("No DESCRIPTION file found")

  stopOrWarn <- function(msg) {
    if (config[["enforceVersionUpdate"]]) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  env <- desc("DESCRIPTION")
  thisVersion <- env$get_version()
  package <- env$get_field("Package")

  if (length(unlist(thisVersion)[-(1:3)])) {
    stopOrWarn(paste0("Version number for packages in development stage found: ", thisVersion))
  }

  if (!is.null(base)) {
    baseVersion <- tryCatch(getBaseVersion(base), error = function(e) e)
    if (inherits(baseVersion, "error")) {
      stopOrWarn(paste0("Failed to read the version from DESCRIPTION in '", base, "': ",
                        conditionMessage(baseVersion)))
    } else if (thisVersion <= baseVersion) {
      stopOrWarn(paste0(
        "Version has not been updated compared to '", base, "'. Please merge the latest changes from '",
        base, "' and run lucode2::buildLibrary() again.\n Base: ", baseVersion, ". Local: ", thisVersion
      ))
    }
  }

  version <- tryCatch(
    available.packages(file.path(repo, "src", "contrib"))[package, "Version"],
    error = function(e) {
      if (config[["enforceVersionUpdate"]]) stop("Failed to retrieve latest version of this package.") else return()
    }
  )

  if (is.null(version)) {
    if (config[["enforceVersionUpdate"]]) stop("Package not found on RSE server.") else return()
  }

  latestVersion <- package_version(version)

  if (thisVersion <= latestVersion) {
    stopOrWarn(paste0(
      "Version has not been updated. Did you run lucode2::buildLibrary()?\n Latest: ",
      latestVersion, ". Local: ", thisVersion
    ))
  }
}

# Read the package version from the DESCRIPTION file in the given git ref.
getBaseVersion <- function(base) {
  gitError <- withr::local_tempfile()
  # "./DESCRIPTION" is relative to the working directory, so this also works for packages in a subfolder of the repo
  baseDescription <- suppressWarnings(system2("git", c("show", shQuote(paste0(base, ":./DESCRIPTION"))),
                                              stdout = TRUE, stderr = gitError))
  status <- attr(baseDescription, "status")
  if (!is.null(status) && status != 0) {
    gitOutput <- readLines(gitError, warn = FALSE)
    stop(paste(c(paste("git show exited with status", status), gitOutput), collapse = "\n"))
  }
  return(desc(text = baseDescription)$get_version())
}
