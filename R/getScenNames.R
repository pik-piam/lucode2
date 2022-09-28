#' getScenNames
#'
#' Get the scenario names (titles) of runs from the specified output folder(s).
#'
#' @param dirs vector of paths to the used output folders.
#' @return A vector containing the titles used as scenario names for e.g. plots
#' @author Lavinia Baumstark
#' @export
getScenNames <- function(dirs) {
  path <- file.path(dirs, "config.Rdata")
  scenarioNames <- NULL
  for (i in path) {
    envir <- new.env()
    load(i, envir = envir)
    scenarioNames[i] <- envir$cfg$title
  }
  return(scenarioNames)
}
