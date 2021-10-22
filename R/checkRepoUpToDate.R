#' @importFrom usethis git_default_branch
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
    withr::local_dir(pathToRepo)

    if (!"upstream" %in% gert::git_remote_list()[["name"]]) {
      remoteUrl <- gert::git_remote_info()[["url"]]
      gert::git_remote_add(url = sub("[^/:]*/", "pik-piam/", remoteUrl), name = "upstream")
    }

    gert::git_fetch("upstream")
    gert::git_fetch()

    behindMessage <- "Your repo is not up-to-date. Please run the following:"
    if (gert::git_ahead_behind(upstream = paste0("upstream/", git_default_branch()))[["behind"]] > 0) {
      behindMessage <- paste0(behindMessage, "\ngit pull upstream ", git_default_branch())
    }
    if (gert::git_ahead_behind()[["behind"]] > 0) {
      behindMessage <- paste0(behindMessage, "\ngit pull")
    }
    if (behindMessage != "Your repo is not up-to-date. Please run the following:") {
      stop(behindMessage)
    } else {
      message("Your repo is up-to-date.")
    }
  }, warning = askUser, error = askUser)
}
