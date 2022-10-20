#' checkDeps
#'
#' Check if package requirements specified in the given DESCRIPTION are met.
#'
#' A warning is thrown for each required package that is not installed, and
#' each installed package whose version number is lower than what is required.
#' Only ">=" version requirements are supported.
#'
#' @param descriptionFile Path to a DESCRIPTION or a path that belongs to a source package project.
#' @param dependencyTypes The types of depencies to check. Must be a
#' subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances").
#' @return Invisibly, a named logical vector indicating whether each package requirement is met.
#' @author Pascal Führlich
#' @examples
#' checkDeps(system.file("DESCRIPTION", package = "lucode2"))
#' @export
checkDeps <- function(descriptionFile = ".", dependencyTypes = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(all(dependencyTypes %in% c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
  allDeps <- desc::desc_get_deps(descriptionFile)
  deps <- allDeps[allDeps$type %in% dependencyTypes, ]
  requirementMet <- Map(checkRequirement, package = deps$package, version = deps$version)
  return(invisible(requirementMet))
}

checkRequirement <- function(package, version) {
  requiredVersion <- package_version(if (version == "*") "0.0" else gsub("[^0-9.]", "", version))

  packageVersion <- tryCatch({
    utils::packageVersion(package)
  }, error = function(error) NA)

  if (package == "R") {
    packageVersion <- getRversion()
  }

  if (is.na(packageVersion)) {
    warning(package, " is required, but not installed - please install it.")
  } else if (packageVersion < requiredVersion) {
    warning(package, " >= ", requiredVersion, " is required, but ", packageVersion,
            " is installed - please update.")
  }

  return(!is.na(packageVersion) && packageVersion >= requiredVersion)
}
