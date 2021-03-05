#' sendmail
#' 
#' A function that sends an automatic email with each push to a gitlab repo
#' 
#' 
#' @param org path to the clone of the gitlab repo
#' @param file absolute path to the file to be commited and pushed
#' @param commitmessage the commit message
#' (appears in the subject line of the email that will be sent)
#' @author Anastasis Giannousakis
#' @export 

sendmail <- function(org,file,commitmessage) {

  if (!grepl("^/",file)) stop("'file' has to be an absolute path!")
  goback<-getwd()
  setwd(org)
  system("git reset --hard origin/master &&
          git pull")
  file.copy(file,"README.md",overwrite=T)
  system(paste0("git commit -m '",commitmessage,"' README.md &&
          git push"))
  setwd(goback)

}
