#' @importFrom utils packageVersion
#' @importFrom withr local_dir
checkRepoUpToDate <- function(pathToRepo = ".", autoCheckRepoUpToDate = TRUE) {
  # asking the user is fallback if automatic check does not work
  askUser <- function(ignoredParameter) { # a warning/error which we don't need is passed by tryCatch
    didYouPullQuestion <- paste("Automatic up-to-date check failed. Is your repository up-to-date?",
                                "Did you pull immediately before running this check? (Y/n) ")
    if (!(tolower(readline(didYouPullQuestion)) %in% c("", "y", "yes"))) {
      stop("Please update your repository first, before you proceed!")
    }
  }

  if (isFALSE(autoCheckRepoUpToDate)) {
    askUser(NULL)
    return(invisible(NULL))
  }

  tryCatch({
    if (packageVersion("usethis") < "2.1.0") {
      message("usethis >= 2.1.0 is needed for automatically checking if repo is up-to-date")
      stop()
    }
    checkRequiredPackages("gert", "automatically checking if the git repo is up-to-date")
    message("Checking if your repository is up-to-date...")
    local_dir(pathToRepo)

    # check whether we are merging
    gitStatus <- system2("git", "status", stdout = TRUE)
    if ("You have unmerged paths." %in% gitStatus ||
        "All conflicts fixed but you are still merging." %in% gitStatus) {
      # gert::git_ahead_behind will say we are behind during merge, so cannot use auto check
      stop()
    }

    if (!"upstream" %in% gert::git_remote_list()[["name"]]) {
      remoteUrl <- sub("[^/:]*/", "pik-piam/", gert::git_remote_info()[["url"]])
      message("Creating a git remote called 'upstream' pointing to ", remoteUrl)
      gert::git_remote_add(url = remoteUrl, name = "upstream")
    }

    gert::git_fetch()
    behindTracking <- gert::git_ahead_behind()[["behind"]]

    gert::git_fetch("upstream")
    behindUpstream <- gert::git_ahead_behind(upstream = paste0("upstream/", usethis::git_default_branch()))[["behind"]]

    if (behindUpstream > 0 || behindTracking > 0) {
      errorMessage <- "Your repo is not up-to-date."
      if (behindTracking > 0) {
        errorMessage <- paste0(errorMessage, "\nYou are ", behindTracking, " commits behind. Please run:\ngit pull")
      }
      if (behindUpstream > 0) {
        errorMessage <- paste0(errorMessage, "\nYou are ", behindUpstream, " commits behind upstream. ",
                               "Please run:\ngit pull upstream ", usethis::git_default_branch())
      }
      stop(errorMessage)
    } else {
      message("Your repo is up-to-date.")
    }
  }, warning = askUser, error = askUser)
}
