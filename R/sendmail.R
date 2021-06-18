#' sendmail
#'
#' A function that sends an automatic email with each push to a gitlab repo
#'
#'
#' @param path path to the clone of the gitlab repo (can be NULL)
#' @param gitrepo if no path is given, the gitlab repo is needed
#' @param file absolute path to the file to be committed
#' @param commitmessage the commit message (appears in the subject line of the email that will be sent)
#' @param remote whether communication with a remote is needed
#' @param reset whether a reset of a local copy is wanted
#' @author Anastasis Giannousakis
#' @export

sendmail <- function(path = NULL, gitrepo, file, commitmessage, remote = FALSE, reset = FALSE) {
  file <- normalizePath(file)

  if (is.null(path)) {
    dir.create(paste0(tempdir(), "/tmp"))
    path <- paste0(tempdir(), "/tmp")
    system(paste0("git clone ", gitrepo, " ", path))
  }

  if (reset) {
    system(paste0("git -C ", path, " reset --hard origin/master")) # nolint
  }
  if (remote) {
    system(paste0("git -C ", path, " pull"))
  }
  file.copy(file, paste0(path, "/README.md"), overwrite = T)
  system(paste0("git -C ", path, " commit -m '", commitmessage, "' README.md"))
  if (remote) {
    system(paste0("git -C ", path, " push"))
  }
}
