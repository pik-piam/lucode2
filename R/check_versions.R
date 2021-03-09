#' Package version check tool
#' 
#' Checks if there are CRAN-packages with the same name as those in our PIK-CRAN whose version is newer
#' 
#' @param mail whether an email notification is sent to RSE
#' @author Anastasis Giannousakis
#' @export
#' @importFrom utils available.packages
#' @examples
#' 
#' \dontrun{check_versions()}
#' 
check_versions <- function(mail=TRUE) {
  
  a<-available.packages("https://rse.pik-potsdam.de/r/packages/src/contrib")
  
  cran<-suppressWarnings(available.packages("https://cloud.r-project.org/src/contrib", filters = "duplicates"))
  
  same <- intersect(rownames(a),rownames(cran))
  
  newer <- NULL
  
  for (i in same) {
    if (a[i,"Version"] > cran[i,"Version"] ) newer[i]<-i
  }
  
  file <- paste0(tempdir(),"/README.md")

  if (length(newer)!=0 ) {
    warning(c("WARNING, following packages have a newer version number on CRAN: ",paste0(newer,collapse = " ")))
    writeLines(c("WARNING, following packages have a newer version number on CRAN: ",paste0(newer,collapse = " ")),file)
    if (mail) sendmail(org="/p/projects/rd3mod/R/libraries/compcrantorse/",file=file,commitmessage="PACKAGE WARNING",remote=TRUE,reset = TRUE)
  } else {
    message("No CRAN packages with newer versions than our RSE packages found\n")
  }

}

