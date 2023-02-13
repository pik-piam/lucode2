#' fixBuildLibraryMergeConflict
#'
#' Fix merge conflicts in files auto-edited by buildLibrary (.buildlibrary, .zenodo.json, DESCRIPTION, README.md).
#'
#' The ValidationKey in .buildlibrary will be set to an empty string and the higher version number
#' in DESCRIPTION is used. buildLibrary needs to be run after this function to deal with the
#' ValidationKey and the merge markers in .zenodo and README.md.
#'
#' @param lib The path to the project with a merge conflict.
#'
#' @importFrom withr local_dir
fixBuildLibraryMergeConflict <- function(lib = ".") {
  local_dir(lib)
  if (!file.exists(".buildlibrary")) {
    return(invisible(NULL))
  }
  lines <- readLines(".buildlibrary")
  if (lines[1] != "<<<<<<< HEAD") {
    # not a buildLibrary merge conflict
    return(invisible(NULL))
  }
  lines <- lines[grep("^(<<<<<<< HEAD|ValidationKey: '[0-9]*'|>>>>>>> [a-z0-9]+)$", lines, invert = TRUE)]
  stopifnot(lines[1] == "=======")
  lines[1] <- "ValidationKey: ''"
  writeLines(lines, ".buildlibrary")

  lines <- readLines("DESCRIPTION")
  versions <- lines %>%
    grep(pattern = "^Version: (.+)$", value = TRUE) %>%
    sub(pattern = "Version: ", replacement = "") %>%
    package_version()

  mergeMarkerPosition <- c(grep("<<<<<<<", lines), grep("=======", lines), grep(">>>>>>>", lines))
  stopifnot(identical(mergeMarkerPosition, sort(mergeMarkerPosition)))
  if (versions[[1]] > versions[[2]]) {
    lines <- lines[-c(mergeMarkerPosition[[1]],
                      mergeMarkerPosition[[2]]:mergeMarkerPosition[[3]])]
  } else {
    lines <- lines[-c(mergeMarkerPosition[[1]]:mergeMarkerPosition[[2]],
                      mergeMarkerPosition[[3]])]
  }

  writeLines(lines, "DESCRIPTION")
}
