#' getFilesToLint
#'
#' Get the R files the current git user is responsible for to pass them to the auto-formatter and/or linter.
#'
#' The files of interest are identified using git via system() (and shell() for windows). All currently untracked files
#' and changed files (both staged and unstaged) are collected, as well as files that were changed in non-merge commits
#' authored by the current git user since the last version commit (a commit where .buildLibrary was changed, presumably
#' to increase the version number). Of those the absolute paths to .R, .Rmd and .Rnw files are returned as a character
#' vector.
#'
#' @param pathToGitRepo path to a git repository
#'
#' @author Pascal Sauer
#' @seealso \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom withr local_dir
#' @examples
#' lucode2:::getFilesToLint()
getFilesToLint <- function(pathToGitRepo = ".") {
  local_dir(pathToGitRepo)
  # use git status to get all changed (untracked, unstaged and staged) files
  recentlyChanged <- substring(system("git status --porcelain", intern = TRUE), 4)

  gitRootPath <- system("git rev-parse --show-toplevel", intern = TRUE)
  lastVersionTime <- system(paste0('git log -1 --format=format:"%at" "', gitRootPath, '/.buildlibrary"'), intern = TRUE)
  lastVersionTime <- as.double(lastVersionTime) + 1 # add 1 second, we want only commits that are actually newer
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    # on windows the global gitconfig is usually not found, so suppress warning
    gitUserMail <- suppressWarnings(system("git config user.email", intern = TRUE))
    if (!identical(attr(gitUserMail, "status"), NULL)) {
      # pass path to global gitconfig explicitly
      gitUserMail <- shell("git config -f %UserProfile%/.gitconfig user.email", intern = TRUE) # nolint
    }
  } else {
    gitUserMail <- system("git config user.email", intern = TRUE)
  }

  # files changed in any non-merge commit authored by current git user since the last version
  previouslyChanged <- system(paste0('git log --name-only --format=format:"" --no-merges ',
                                     '--since="', lastVersionTime, '" --author="', gitUserMail, '"'), intern = TRUE)

  # remove duplicate files, keep only .R .Rmd .Rnw files
  filesToLint <- grep(pattern = "\\.R(md|nw)?$", x = unique(c(recentlyChanged, previouslyChanged)), value = TRUE)
  if (length(filesToLint) > 0) {
    # prepend git root path, so we get absolute paths
    filesToLint <- paste0(gitRootPath, "/", filesToLint)
  }

  # remove files that do not exist or we lack read permission for
  filesToLint <- filesToLint[file.access(filesToLint, mode = 4) == 0]
  return(filesToLint)
}
