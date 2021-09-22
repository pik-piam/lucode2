#' validkey
#'
#' Support function which validates a key out of a version date combination
#'
#' This function is used to check whether \code{\link{buildLibrary}} has been run
#' properly and without problems or not
#'
#' @param package Path to the package
#' @param stopIfInvalid logical; whether to stop if the key is invalid.
#' @return list with version, date and result of validation test
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
validkey <- function(package = ".", stopIfInvalid = FALSE) {
  file <- paste0(package, "/DESCRIPTION")
  if (!file.exists(file)) return(list(version = 0, date = 0, roxygen = FALSE, valid = FALSE))
  descfile <- readLines(file, warn = FALSE)
  .read <- function(x, y) {
    return(sub("[^(0-9)]*$", "", sub(paste0(x, ":[^(0-9)]*"), "", grep(x, y, value = T), perl = T), perl = T))
  }
  version <- .read("Version", descfile)
  date <- as.Date(.read("Date", descfile))
  cfgfile <- paste0(package, "/.buildlibrary")
  if (file.exists(cfgfile)) {
    vkey <- as.numeric(read_yaml(cfgfile)$ValidationKey)
  } else {
    vkey <- as.numeric(.read("ValidationKey", descfile))
  }
  out <- list(version = version, date = date, roxygen = any(grepl("RoxygenNote", descfile)))
  if (length(vkey) == 0) {
    out$valid <- FALSE
  } else {
    out$valid <- (vkey == validationkey(version, date))
  }
  if (stopIfInvalid && !out[["valid"]]) {
    stop(paste("The ValidationKey in .buildLibrary is invalid. Not all changes made by lucode2::buildLibrary()",
               "were commited, or it was not run at all."))
  }
  return(out)
}
