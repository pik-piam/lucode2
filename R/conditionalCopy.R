#' @importFrom tools md5sum
#' @importFrom utils askYesNo
conditionalCopy <- function(relativePath, nameInInstExtdata = basename(relativePath)) {
  if (!file.exists("DESCRIPTION")) {
    stop("No DESCRIPTION file found in ", normalizePath("."))
  }
  instExtdataPath <- file.path("inst", "extdata", nameInInstExtdata)
  if (desc("DESCRIPTION")$get("Package") == "lucode2") {
    if (file.exists(instExtdataPath) && md5sum(relativePath) != md5sum(instExtdataPath) &&
        (!interactive() || !askYesNo(paste0("Replace ", instExtdataPath, " with ", relativePath, "?")))) {
      stop(relativePath, " != ", instExtdataPath)
    }
    file.copy(relativePath, instExtdataPath, overwrite = TRUE)
  } else {
    file.copy(system.file("extdata", nameInInstExtdata, package = "lucode2"), relativePath)
  }
  return(invisible(NULL))
}
