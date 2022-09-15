#' conditionalCopy
#'
#' Copy a file from lucode2 into the current package.
#'
#' For normal packages, it will simply overwrite the given file from the corresponding
#' file in lucode2's extdata.
#' For lucode2 itself, it instead checks if the file in extdata matches the file in
#' the main folder. If not, it asks if the file in extdata should be updated.
#'
#' @param relativePath The destination to copy to.
#' @param nameInInstExtdata The source file name in lucode2's extdata, if it differs from relativePath's basename.
#'
#' @importFrom tools md5sum
#' @importFrom utils askYesNo
conditionalCopy <- function(relativePath, nameInInstExtdata = basename(relativePath)) {
  if (!file.exists("DESCRIPTION")) {
    stop("No DESCRIPTION file found in ", normalizePath("."))
  }
  instExtdataPath <- file.path("inst", "extdata", nameInInstExtdata)
  dir.create(dirname(relativePath), recursive = TRUE, showWarnings = FALSE)
  if (desc("DESCRIPTION")$get("Package") == "lucode2") {
    if (file.exists(instExtdataPath) && md5sum(relativePath) != md5sum(instExtdataPath) &&
        (!interactive() || (requireNamespace("testthat", quietly = TRUE) && testthat::is_testing()) ||
         !askYesNo(paste0("Replace ", instExtdataPath, " with ", relativePath, "?")))) {
      stop(relativePath, " != ", instExtdataPath)
    }
    file.copy(relativePath, instExtdataPath, overwrite = TRUE)
  } else {
    file.copy(system.file("extdata", nameInInstExtdata, package = "lucode2"), relativePath, overwrite = TRUE)
  }
  return(invisible(NULL))
}
