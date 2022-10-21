#' checkDeps
#'
#' Check if package requirements specified in the given DESCRIPTION are met.
#'
#' A warning is thrown for each required package that is not installed, and
#' each installed package whose version does not meet the requirements.
#'
#' @param descriptionFile Path to a DESCRIPTION or a path that belongs to a source package project.
#' @param dependencyTypes The types of depencies to check. Must be a
#' subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances").
#' @return Invisibly, a named logical vector indicating whether each package requirement is met.
#' @author Pascal Führlich
#' @examples
#' checkDeps(system.file("DESCRIPTION", package = "lucode2"))
#'
#' @importFrom methods getFunction
#'
#' @export
checkDeps <- function(descriptionFile = ".", dependencyTypes = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(all(dependencyTypes %in% c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
  allDeps <- desc::desc_get_deps(descriptionFile)
  deps <- allDeps[allDeps$type %in% dependencyTypes, ]
  requirementMet <- Map(checkRequirement, package = deps$package, version = deps$version)
  return(invisible(requirementMet))
}

checkRequirement <- function(package, version) {
  # get installed package or R version
  if (package == "R") {
    packageVersion <- getRversion()
  } else {
    packageVersion <- tryCatch({
      utils::packageVersion(package)
    }, error = function(error) NA)
  }

  # if the package is not installed
  if (is.na(packageVersion)) {
    warning(package, " is required, but not installed - please install it.")
    result <- FALSE
  # if we don"t care about the version
  } else if ("*" == version) {
    result <- TRUE
  # compare version requirement
  } else {
    validops <- c("<", "<=", ">", ">=", "==", "!=")

    # split version into operator and version number
    opANDv <- unlist(
      regmatches(x = version,
                 m = gregexpr(pattern = paste0("(",
                                               paste(c(validops, "[0-9\\.-]+"),
                                                     collapse = "|"),
                                               ")"),
                              text = version))
    )

    # compare to installed version
    if (opANDv[1] %in% validops) {
      result <- getFunction(name = opANDv[1])(packageVersion,
                                            package_version(opANDv[2]))

      # warn on mismatch
      if (!result)
        warning(paste(package, opANDv[1], package_version(opANDv[2]),
                      "is required, but", packageVersion,
                      "is installed - please update."))
    # catch faulty declarations
    } else {
      stop(paste("invalid dependency declatation:", package, version))
    }
  }
  return(result)
}
