#' incrementVersion
#'
#' Increment a version number at the specified position.
#'
#' @param currentVersion The current package version as a string like "1.23.456"
#' @param position An integer defining which part of the version number will be increased. Use 1 for major version,
#'                 2 for minor, 3 for patch/bugfix, 4 for development.
#' @param defLengths An integer defining how many parts make up the resulting version number.
#' @return The new version string.
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal Sauer
#' @seealso \code{\link{buildLibrary}}
#' @examples
#' lucode2:::incrementVersion("1.23.45", 3)
incrementVersion <- function(currentVersion, position, defLengths = 3) {
  currentVersion <- numeric_version(currentVersion)
  if (position == 0) {
    return(currentVersion)
  }
  for (i in seq_len(position)) {
    if (is.na(currentVersion[1, i])) {
      currentVersion[1, i] <- 0
    }
  }
  if (currentVersion[1, position] == "0" && position == 4) {
    currentVersion[1, position] <- 9000
  }

  currentVersion[1, position] <- as.numeric(currentVersion[1, position]) + 1
  if (defLengths > position) {
    for (i in (position + 1):defLengths) {
      currentVersion[1, i] <- 0
    }
  }
  currentVersion <- currentVersion[1, seq_len(max(position, defLengths))]
  return(currentVersion)
}
