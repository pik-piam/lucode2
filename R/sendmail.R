#' sendmail
#' 
#' A function that sends an automatic email with each push to a gitlab repo
#' 
#' 
#' @param org path to the clone of the gitlab repo (can be NULL)
#' @param gitrepo if no path is given, the gitlab repo is needed
#' @param file absolute path to the file to be committed
#' @param commitmessage the commit message
#' @param remote whether communication with a remote is needed
#' @param reset whether a reset of a local copy is wanted
#' (appears in the subject line of the email that will be sent)
#' @author Anastasis Giannousakis
#' @export 

sendmail <- function(org=NULL,gitrepo,file,commitmessage,remote=FALSE,reset=FALSE) {

  file <- normalizePath(file)

  if (is.null(org)) {
    dir.create(paste0(tempdir(),"/tmp"))
    org <- paste0(tempdir(),"/tmp")
    system(paste0("git clone ",gitrepo," ",org))
  }

  if (reset) system(paste0("git -C ",org," reset --hard origin/master"))
  if (remote) system(paste0("git -C ",org," pull"))
  file.copy(file,paste0(org,"/README.md"),overwrite=T)
  system(paste0("git -C ",org," commit -m '",commitmessage,"' README.md"))
  if (remote) system(paste0("git -C ",org," push"))


}
