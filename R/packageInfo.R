#' packageInfo
#'
#' Function to print version number and time since last update formatted to standard output.
#' Considers CRAN, the RSE server, and r-universe.
#'
#' @param package Package name
#' @param repos vector of package repositories in which availability of the package should
#' be checked
#' @author Jan Philipp Dietrich
#' @importFrom utils packageVersion available.packages
#' @export
packageInfo <- function(package, repos = c("https://cran.rstudio.com/",
                                           "https://rse.pik-potsdam.de/r/packages/",
                                           "https://pik-piam.r-universe.dev")) {
  version <- try(packageVersion(package), silent = TRUE)
  if ("try-error" %in% class(version)) {
    version <- "<not installed>"
    installed <- "<never>"
  } else {
    version <- as.character(version)
    installed <- paste(
      as.integer(difftime(Sys.time(), file.mtime(system.file("DESCRIPTION", package = package)), units = "mins")),
      "minutes ago")
  }

  cat("\nPackage:", package, "\n")
  cat("Installed version:", version, "\n")
  cat("Last updated:", installed, "\n\n")

  if (!is.null(repos)) {
    cat("Availability in repositories:\n")
    v <- list()
    for (r in repos) {
      v$r <- tryCatch(
        available.packages(file.path(r, "src", "contrib"))[package, "Version"],
        error = function(e) return("<not available>"))
      cat(v$r, "|", r, "\n")
    }
    cat("\n")
  }
}
