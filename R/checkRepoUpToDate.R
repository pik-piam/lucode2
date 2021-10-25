#' @importFrom usethis git_default_branch
#' @importFrom withr local_dir
checkRepoUpToDate <- function(pathToRepo = ".") {
  # asking the user is fallback if automatic check does not work
  askUser <- function(ignoredParameter) {
    didYouPullQuestion <- paste("Automatic up-to-date check failed. Is your repository up-to-date?",
                                "Did you pull immediately before running this check? (Y/n) ")
    if (!(tolower(readline(didYouPullQuestion)) %in% c("", "y", "yes"))) {
      stop("Please update your repository first, before you proceed!")
    }
  }

  tryCatch({
    checkRequiredPackages("gert", "automatically checking if the git repo is up-to-date")
    message("Checking if your repository is up-to-date...")
    local_dir(pathToRepo)

    if (!"upstream" %in% gert::git_remote_list()[["name"]]) {
      remoteUrl <- sub("[^/:]*/", "pik-piam/", gert::git_remote_info()[["url"]])
      message("Creating a git remote called 'upstream' pointing to ", remoteUrl)
      gert::git_remote_add(url = remoteUrl, name = "upstream")
    }

    gert::git_fetch()
    behindTracking <- gert::git_ahead_behind()[["behind"]]

    gert::git_fetch("upstream")
    behindUpstream <- gert::git_ahead_behind(upstream = paste0("upstream/", git_default_branch()))[["behind"]]

    if (behindUpstream > 0 || behindTracking > 0) {
      errorMessage <- "Your repo is not up-to-date."
      if (behindTracking > 0) {
        errorMessage <- paste0(errorMessage, "\nYou are ", behindTracking, " commits behind. Please run:\ngit pull")
      }
      if (behindUpstream > 0) {
        errorMessage <- paste0(errorMessage, "\nYou are ", behindUpstream, " commits behind upstream. ",
                               "Please run:\ngit pull upstream ", git_default_branch())
      }
      stop(errorMessage)
    } else {
      message("Your repo is up-to-date.")
    }
  }, warning = askUser, error = askUser)
}
