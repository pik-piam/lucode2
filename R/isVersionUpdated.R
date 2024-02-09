#' isVersionUpdated
#'
#' Checks if the version number in the DESCRIPTION file of a given package has
#' been updated.

#' @importFrom desc desc
#' @importFrom utils packageVersion
#' @author Falk Benke
#' @export
isVersionUpdated <- function() {
  if (!file.exists("DESCRIPTION")) stop("No DESCRIPTION file found")

  env <- desc("DESCRIPTION")
  thisVersion <- env$get_version()
  package <- env$get_field("Package")

  version <- tryCatch(
    available.packages(file.path("https://rse.pik-potsdam.de/r/packages/", "src", "contrib"))[package, "Version"],
    error = function(e) {
      stop("Failed to retrieve latest version of this package.")
    }
  )
  latestVersion <- package_version(version)

  if (thisVersion <= latestVersion) {
    stop(paste0("Version has not been updated. Did you run lucode2::buildLibrary()?\n Latest: ", latestVersion))
  }
}
