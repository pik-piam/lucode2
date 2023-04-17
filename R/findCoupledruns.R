#' findCoupledruns
#'
#' Extracts scenario names from coupled runs in the given outputfolder. The
#' scenario names will be extracted based on the folder names of the results
#' folders.
#'
#'
#' @param resultsfolder Path to an output folder.
#' @return A vector containing the names of the scenarios.
#' @author David Klein
#' @export
findCoupledruns <- function(resultsfolder) {
  cat("\nSearching for all scenarios of coupled runs in", resultsfolder, ": ")
  # Find which runs were performed by searching for all files that contain "-rem-"
  runs <- list.files(resultsfolder, "[^[:alnum:]_-]*-rem-[0-9]+$")
  if (length(runs) == 0) {
    runs <- list.files(resultsfolder, "[^[:alnum:]_-]*-mag-[0-9]+$")
  }
  # keep directories only (filter out files)
  runs <- runs[file.info(file.path(resultsfolder, runs))[, "isdir"]]
  # Remove "-rem-*" from the folder names and remove remaining double elements to yield the pure runname
  runs <- unique(sub("-(rem|mag)-[0-9]+", "", runs))
  message(length(runs), " scenarios found.\n")
  return(runs)
}
