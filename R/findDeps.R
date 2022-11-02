#' findDeps
#'
#' Find all dependencies of an R project.
#'
#' This is a wrapper around `renv::dependencies()` that does not report
#' dependencies on core R packages, because these are always available.
#'
#' @param devDeps Whether development dependencies should also be checked.
#' @return A dataframe documenting which dependency is needed where.
#' @author Pascal Führlich
#' @seealso \code{\link[renv]{dependencies}}
#' @importFrom utils installed.packages
#' @export
findDeps <- function(devDeps = TRUE) {
  checkRequiredPackages("renv")
  # let renv auto-detect dependencies, including dev dependencies
  deps <- renv::dependencies(dev = devDeps)
  # remove packages that are part of core R
  corePackages <- installed.packages(priority = "base")[, "Package"]
  deps <- deps[!deps$Package %in% corePackages, ]
  cat(paste0('c("', paste0(sort(unique(deps$Package)), '"', collapse = ',\n  "'), ")\n"))
  return(deps)
}
