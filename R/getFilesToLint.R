#' getFilesToLint
#'
#' Get the R files the current git user is responsible for to pass them to the auto-formatter and/or linter.
#'
#' The files of interest are identified using git via system(). All currently untracked files and changed files (both
#' staged and unstaged) are collected, as well as files that were changed in a non-merge commit authored by the current
#' git user within the last 30 days. Of those the absolute paths to .R, .Rmd and .Rnw files are returned as a character
#' vector.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom dplyr %>%
#' @examples
#' lucode2:::getFilesToLint()
getFilesToLint <- function() {
  # untracked, unstaged, staged files
  recentlyChanged <- system("git status --porcelain", intern = TRUE) %>% # get git status in parsable form
    (function(x) grep("^ ?D", x, value = TRUE, invert = TRUE)) %>% # filter out deleted files
    substring(4) # trim info on modified/added/staged

  # files changed in any non-merge commit authored by current git user within the last 30 days
  previouslyChanged <- system(paste0('git log --name-only --format=format:"" --no-merges --since="30 days"',
                                     ' --author="', system("git config user.email", intern = TRUE), '"'),
                              intern = TRUE)

  # remove duplicate files, keep only .R .Rmd .Rnw files
  filesToLint <- grep(pattern = "\\.R(md|nw)?$", x = unique(c(recentlyChanged, previouslyChanged)), value = TRUE)
  if (length(filesToLint) > 0) {
    # prepend git root path, so we get absolute paths
    filesToLint <- paste0(system("git rev-parse --show-toplevel", intern = TRUE), "/", filesToLint)
  }
  return(filesToLint)
}
