#' localCranMirror
#'
#' Use a fallback CRAN mirror if none is configured, i.e. if the CRAN entry of getOption("repos") is still
#' the placeholder "@CRAN@". The repos option is restored when the calling function exits.
#'
#' @param mirror The CRAN mirror to use if none is configured.
#' @param envir The environment whose exit restores the repos option, defaults to the calling function.
#'
#' @importFrom withr local_options
localCranMirror <- function(mirror = "https://cloud.r-project.org", envir = parent.frame()) {
  repos <- getOption("repos")
  if (isTRUE(unname(repos["CRAN"]) == "@CRAN@")) {
    repos["CRAN"] <- mirror
    local_options(repos = repos, .local_envir = envir)
  }
  return(invisible(getOption("repos")))
}
