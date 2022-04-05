#' @importFrom utils packageVersion
#' @importFrom usethis local_project git_default_branch
checkRepoUpToDate <- function(pathToRepo = ".", autoCheckRepoUpToDate = TRUE) {
  # asking the user is fallback if automatic check does not work
  askUser <- function() {
    didYouPullQuestion <- paste("Automatic up-to-date check failed/skipped. Is your repository up-to-date?",
                                "Did you pull immediately before running this check? (Y/n) ")
    if (!(tolower(readline(didYouPullQuestion)) %in% c("", "y", "yes"))) {
      stop("Please update your repository first, before you proceed!")
    }
  }

  if (isFALSE(autoCheckRepoUpToDate)) {
    askUser()
    return(invisible(NULL))
  }

  checkRequiredPackages("gert", "auto-check if git repo is up-to-date")
  message("Checking if your repo is up-to-date...")
  local_project(pathToRepo, quiet = TRUE)

  # check whether we are merging
  gitStatus <- system2("git", "status", stdout = TRUE)
  if ("You have unmerged paths." %in% gitStatus ||
      "All conflicts fixed but you are still merging." %in% gitStatus) {
    # gert::git_ahead_behind will say we are behind during merge, so cannot use auto check
    askUser()
    return(invisible(NULL))
  }

  if ("upstream" %in% gert::git_remote_list()[["name"]] &&
      startsWith(gert::git_remote_info("upstream")[["url"]], "https:pik-piam")) {
    gert::git_remote_remove("upstream")
  }

  if (!"upstream" %in% gert::git_remote_list()[["name"]]) {
    remoteUrl <- sub("[^/:]+/([^/]+$)", "pik-piam/\\1", gert::git_remote_info()[["url"]])
    message("Creating a git remote called 'upstream' pointing to ", remoteUrl)
    gert::git_remote_add(url = remoteUrl, name = "upstream")
  }

  gert::git_fetch(verbose = FALSE)
  behindTracking <- gert::git_ahead_behind()[["behind"]]

  gert::git_fetch("upstream", verbose = FALSE)
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
}
