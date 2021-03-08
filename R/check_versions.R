#' Library cleanup tool
#' 
#' Checks if there are CRAN-packages with the same name as those in our PIK-CRAN whose version is newer
#' 
#' @param mail whether an email notification is sent to RSE
#' @author Anastasis Giannousakis
#' @export
#' @importFrom utils available.packages old.packages
#' @examples
#' 
#' \dontrun{check_versions()}
#' 
check_versions <- function(mail=TRUE) {
  
  a<-rownames(available.packages("https://rse.pik-potsdam.de/r/packages/src/contrib"))
  
  cran<-rownames(old.packages(repos=getOption("repos")["CRAN"]))
  
  newer <- intersect(a,cran)
  file <- paste0(tempdir(),"/README.md")

  if (length(newer)!=0 ) {
    cat("WARNING\n")
    writeLines(paste0("WARNING, following packages have a newer version number on CRAN: ",newer),file)
    if (mail) sendmail("/p/projects/rd3mod/R/libraries/compcrantorse/",file,commitmessage="PACKAGE WARNING")
  } else {
    cat("No CRAN packages with newer versions than our RSE packages found\n")
  }

}

