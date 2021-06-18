#' findIterations
#'
#' Collects paths to all coupled runs (iterations) in \code{modelpath} that
#' contain \code{runname}. For each entry in \code{runname} the paths are
#' sorted by the modification time of the respective fulldata.gdx
#'
#'
#' @param runname Scenarioname or vector of scenarionames.
#' @param modelpath Path or vector of paths where iterations are searched for.
#' @param latest Logical indicating if only the latest iteration of a runname
#' is returned.
#' @return A vector containing the paths to the iterations of coupled runs.
#' @author David Klein
#' @export
#' @importFrom utils tail
findIterations <- function(runname, modelpath = ".", latest = FALSE) {
  gdxPath <- NULL
  for (run in runname) {
    cat("\nSearching for iterations of coupled scenario", run, ": ")
    # collect all paths to fulldata.gdx that contain "-rem-" or "-mag-"
    gdxPathPerRun <- NULL
    for (p in modelpath) {
      # Using Sys.glob you can not avoid that the pattern "SSP2-ref-*/fulldata.gdx"
      # finds both "SSP2-ref-rem-[1-8]" AND "SSP2-ref-T-rem-[1-8]". Because Sys.glob
      # only handles wildcards but no regular expressions there is no way to tell
      # Sys.glob that I should look for something like "SSP2-ref-(rem|mag)*/fulldata.gdx".
      # Therefore, we have to append "-rem-" and "-mag-" to the search pattern and
      # execute Sys.glob twice.
      patternRemind <- paste0(p, "/", run, "-rem-*/fulldata.gdx")
      patternMagpie <- paste0(p, "/", run, "-mag-*/fulldata.gdx")
      pattern <- c(patternRemind, patternMagpie)
      tmp <- Sys.glob(pattern)
      ind <- grep("-(rem|mag)-", tmp)
      gdxPathPerRun <- c(gdxPathPerRun, tmp[ind])
    }

    # sort paths by modification date of fulldata.gdx
    tmp <- file.info(gdxPathPerRun)
    tmp <- tmp[with(tmp, order(mtime)), ] # nolint
    tmp <- rownames(tmp) # take path
    if (latest) tmp <- tail(tmp, n = 1)
    cat(length(tmp), "runs found.")
    gdxPath <- c(gdxPath, tmp)
  }

  gdxPath <- sub("/fulldata.gdx", "", gdxPath)
  cat("\nIn total", length(gdxPath), "runs found.\n")
  return(gdxPath)
}
