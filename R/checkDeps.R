#' Check Dependencies
#'
#' Check if package requirements specified in the given DESCRIPTION are met.
#'
#' @md
#' @param descriptionFile Path to a DESCRIPTION file or a path that belongs to a
#'   source package project.
#' @param dependencyTypes The types of dependencies to check. Must be a
#' subset of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#' @param action Action to take on unmet dependencies:
#'   - `"stop"`:  Issue an error with the unmet dependencies.  (Default.)
#'   - `"warn"`:  Issue a warning with the unmet dependencies.
#'   - `"pass"`:  Do nothing, just return invisibly.
#' @return Invisibly, a named character vector indicating whether each package
#'   requirement is met (`"TRUE"`) or not, in which case the reason is stated.
#'
#' @author Pascal Führlich
#'
#' @examples
#' checkDeps(system.file("DESCRIPTION", package = "lucode2"))
#'
#' @importFrom methods getFunction
#'
#' @export
checkDeps <- function(descriptionFile = ".",
                      dependencyTypes = c("Depends", "Imports", "LinkingTo"),
                      action = "stop") {
  stopifnot(all(dependencyTypes %in% c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
  stopifnot(action %in% c("stop", "warn", "pass"))
  allDeps <- desc::desc_get_deps(descriptionFile)
  deps <- allDeps[allDeps$type %in% dependencyTypes, ]
  requirementMet <- Map(checkRequirement, package = deps$package, version = deps$version)

  if (!all("TRUE" == requirementMet)) {
    message <- paste(requirementMet[which("TRUE" != requirementMet)],
                     collapse = "\n  ")
    if ("stop" == action) {
      stop(message)
    } else if ("warn" == action) {
      warning(message)
    }
  }

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
    result <- paste(package,
                    "is required, but not installed - please install it.")
  # if we don't care about the version
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
      if (getFunction(name = opANDv[1])(packageVersion,
                                            package_version(opANDv[2]))) {
        result <- TRUE
        # warn on mismatch
      } else {
        result <- paste(package, opANDv[1], package_version(opANDv[2]),
                      "is required, but", packageVersion,
                      "is installed - install a compatible version.")
      }
    # catch faulty declarations
    } else {
      result <- paste("invalid dependency declaration:", package, version)
    }
  }
  return(as.character(result))
}
