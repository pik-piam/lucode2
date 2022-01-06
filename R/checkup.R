#' checkup
#'
#' Checks the current R ecosystem for common problems and reports info such as OS and R version.
#'
#' @return Invisibly the report as a list.
#'
#' @author Pascal Führlich
#'
#' @importFrom tools md5sum
#' @importFrom utils old.packages str tail
#' @export
checkup <- function() {
  rscriptVersion <- tail(system("Rscript -e 'cat(R.version.string)'", intern = TRUE), 1)
  if (rscriptVersion != R.version.string) {
    warning("Rscript uses ", rscriptVersion, ", but this session uses ", R.version.string, "")
  }

  outdatedPackages <- old.packages()
  if (length(outdatedPackages) > 0) {
    warning("The following packages can be updated:\n",
            paste("-", outdatedPackages[, "Package"], ":", outdatedPackages[, "Installed"], "->",
                  outdatedPackages[, "ReposVer"], collapse = "\n"),
            "\nConsider running `update.packages()`.")
  }

  packagesInLibPaths <- lapply(.libPaths(), list.files) # nolint
  names(packagesInLibPaths) <- .libPaths() # nolint
  duplicatePackages <- findDuplicates(packagesInLibPaths)
  if (length(duplicatePackages) > 0) {
    warning("The following packages are installed in multiple libPaths:\n",
            paste0(names(duplicatePackages), ": ", duplicatePackages, collapse = "\n"),
            "Consider uninstalling one version via `remove.packages('packageName', lib = 'libPath')`")
  }

  pathEnvironmentVariable <- unique(strsplit(Sys.getenv("PATH"), ":")[[1]])
  filesInPath <- lapply(pathEnvironmentVariable, list.files)
  names(filesInPath) <- pathEnvironmentVariable
  duplicateFilesInPath <- findDuplicates(filesInPath)
  stopifnot(length(unique(names(duplicateFilesInPath))) == length(duplicateFilesInPath))
  duplicateDifferingFilesInPath <- duplicateFilesInPath[vapply(names(duplicateFilesInPath), function(programName) {
    return(tryCatch({
      length(unique(md5sum(file.path(duplicateFilesInPath[[programName]], programName)))) >= 2
    }, warning = function(unused) FALSE, error = function(unused) FALSE))
  }, logical(1))]
  duplicateDifferingFilesInPath
  if (length(duplicateDifferingFilesInPath) > 0) {
    warning("Multiple different versions of the following programs are found in your PATH environment variable:\n",
            paste0(names(duplicateDifferingFilesInPath), ": ", duplicateDifferingFilesInPath, collapse = "\n"),
            "\nConsider uninstalling all but one version of each program to remove ambiguity.")
  }

  report <- R.Version()[c("platform", "version.string")]
  report[["repos"]] <- as.character(getOption("repos"))

  expectedRepos <- c("https://pik-piam.r-universe.dev", "https://rse.pik-potsdam.de/r/packages")
  if (length(intersect(report[["repos"]], expectedRepos)) == 0) {
    warning("No PIK CRAN in getOption('repos'). Consider adding the RSE server and/or r-universe by running\n",
            paste0("options(repos = c(getOption('repos'), '", expectedRepos, "'))", collapse = "\n"))
  }

  report[["gams"]] <- FALSE
  if (requireNamespace("gdx", quietly = TRUE) && requireNamespace("gdxrrw", quietly = TRUE)) {
    gdx:::.onLoad(NULL, NULL) # initialize gams # nolint
    report[["gams"]] <- gdxrrw::igdx(silent = TRUE, returnStr = TRUE)
  }

  report[["pandoc"]] <- if (requireNamespace("rmarkdown", quietly = TRUE))
    as.character(rmarkdown::pandoc_version())
  else
    "? (Run `install.packages('rmarkdown')` to enable pandoc check.)"

  str(report)
  return(invisible(report))
}
