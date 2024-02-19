#' isVersionUpdated
#'
#' Checks if the version number in the DESCRIPTION file of a given package has
#' been updated.
#' @param repo package repository to determine latest version
#' @param config A configuration defining enforceVersionUpdate. By default
#' the .buildLibrary file is read.
#' @importFrom desc desc
#' @importFrom utils packageVersion
#' @author Falk Benke
#' @export
isVersionUpdated <- function(repo = "https://rse.pik-potsdam.de/r/packages/",
                             config = loadBuildLibraryConfig()) {
  if (!file.exists("DESCRIPTION")) stop("No DESCRIPTION file found")

  env <- desc("DESCRIPTION")
  thisVersion <- env$get_version()
  package <- env$get_field("Package")

  version <- tryCatch(
    available.packages(file.path(repo, "src", "contrib"))[package, "Version"],
    error = function(e) {
      stop("Failed to retrieve latest version of this package.")
    }
  )
  latestVersion <- package_version(version)

  if (thisVersion <= latestVersion) {
    msg <- paste0("Version has not been updated. Did you run lucode2::buildLibrary()?\n Latest: ",
                  latestVersion, ". Local: ", thisVersion)

    if (config[["enforceVersionUpdate"]]) stop(msg) else warning(msg)

  }
}
