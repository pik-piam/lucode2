#' sendmail
#' 
#' A function that sends an automatic email with each push to a gitlab repo
#' 
#' 
#' @param org path to the clone of the gitlab repo
#' @param file absolute path to the file to be commited and pushed
#' @author Anastasis Giannousakis
#' @export 

sendmail <- function(org,file) {

  if (!grepl("^/",file)) stop("'file' has to be an absolute path!")
  goback<-getwd()
  setwd(org)
  system("git reset --hard origin/master &&
          git pull")
  file.copy(file,"README.md",overwrite=T)
  system("git commit -m 'mycommit' README.md &&
          git push")
  setwd(goback)

}
