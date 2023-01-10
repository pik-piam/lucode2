#' @importFrom utils packageVersion
#' @importFrom usethis local_project git_default_branch
checkRepoUpToDate <- function(pathToRepo = ".", autoCheckRepoUpToDate = TRUE) {
  if (is.null(autoCheckRepoUpToDate)) {
    return(invisible(NULL))
  } else if (isFALSE(autoCheckRepoUpToDate)) {
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
    message("Automatic repo up-to-date check does not work during merge.")
    askUser()
    return(invisible(NULL))
  }

  upstreamExists <- function() "upstream" %in% gert::git_remote_list()[["name"]]

  if (!upstreamExists()) {
    remoteUrl <- sub("[^/:]+/([^/]+$)", "pik-piam/\\1", gert::git_remote_list()[[1, "url"]])
    gert::git_remote_add(url = remoteUrl, name = "upstream")
    if (fetch("upstream", silent = TRUE)) {
      message("Created a git remote called 'upstream' pointing to ", remoteUrl)
    } else {
      gert::git_remote_remove("upstream")
    }
  }

  behindTracking <- 0
  branchList <- gert::git_branch_list()
  # check if a remote tracking branch is configured for the current branch
  if (!is.na(branchList[branchList[, "name"] == gert::git_branch(), "upstream"][[1, 1]])) {
    if (!fetch()) {
      message("Automatic repo up-to-date check could not fetch from git remote.")
      askUser()
      return(invisible(NULL))
    }
    behindTracking <- gert::git_ahead_behind()[["behind"]]
  }

  if (upstreamExists() && !fetch("upstream")) {
    message("Automatic repo up-to-date check could not fetch from git remote.")
    askUser()
    return(invisible(NULL))
  }
  referenceBranch <- if (upstreamExists()) paste0("upstream/", git_default_branch()) else NULL
  behindUpstream <- gert::git_ahead_behind(referenceBranch)[["behind"]]

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

# asking the user is fallback if automatic check does not work
askUser <- function() {
  message("Is your repo up-to-date? Did you pull from the upstream",
          "repo immediately before running this check? (Y/n) ", appendLF = FALSE)
  if (!(tolower(getLine()) %in% c("", "y", "yes"))) {
    stop("Please update your repository first, before you proceed!")
  }
  message()
}

fetch <- function(remote = NULL, silent = FALSE) {
  return(tryCatch({
    gert::git_fetch(remote, verbose = FALSE)
    TRUE
  }, error = function(error) {
    if (Sys.which("git") == "") {
      return(FALSE)
    }
    systemOutput <- if (silent) FALSE else ""
    exitCode <- system2("git", c("fetch", remote), stdout = systemOutput, stderr = systemOutput)
    return(exitCode == 0)
  }))
}
