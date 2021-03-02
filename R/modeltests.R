#' modeltests
#'
#' Runs a group of tests for a specific model.
#'
#'
#' @param dir Path to the folder where the model is found
#' @param gitdir path to the git clone that sends the report via email
#' @param user the user that starts the job and commits the changes
#' @param model Model name
#'
#'
#' @author Anastasis Giannousakis
#' @seealso \code{\link{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom iamc iamCheck
#' @importFrom quitte read.quitte
#' @export
modeltests<-function(dir=".",gitdir=NULL, model=NULL,user=NULL){


  if (is.null(model)) stop("Model cannot be NULL")

  setwd(dir)
  system("git reset --hard origin/develop && git pull")
  if (any(grepl("choose_slurmConfig",readLines("start.R")))) file.copy("/p/projects/remind/start.R","start.R",overwrite=T)
  source("start.R")
  out<-list()
  modelinerror = FALSE

  runcode<- paste0("-AMT-.*.202[1-9]-[0-1][0-9]-",format(Sys.Date(),"%d"))
  repeat {
    if(!any(grepl(runcode,system(paste0("squeue -u ",user," -h -o '%i %q %T %C %M %j %V %L %e %Z'"),intern=TRUE) ))) break
  }

  expr <- paste0("'/.*.",runcode,"_[0-9][0-9].[0-9][0-9].[0-9][0-9]'")
  paths <- system(paste0("find ",dir,"/output -type d -regex ",expr),intern=TRUE)

  # Did all the runs start?
  amtcsv<-read.csv2(paste0(dir,"/config/scenario_config_AMT.csv"))
  start<-NULL
  amtcsv <- filter(amtcsv,start==1)
  if (length(paths) < length(amtcsv[,"start"]) ) out["Runs"]<-"Some runs did not start"

  # Which runs finished properly?
  mifs <- system(paste0("find /p/projects/remind/modeltests/ -name ",model,"*mif"),intern=TRUE)
  mifs <- grep(runcode,mifs,value=TRUE)
  mifs <- grep("_withoutPlus",mifs,invert = TRUE, value = TRUE)
  if (length(mifs) < length(amtcsv[,"start"]) ) out["mifs"]<-"Some runs did not write a reporting file"

  a <- read.quitte(mifs)
  out[["iamCheck"]] <- iamCheck(a,model)


#  write.csv(c(date(),out[["Runs"]]),file = "README.md")
  rdm<-readLines(paste0(gitdir,"/README_temp.md"))
  rdm <- sub("Date: .*",paste0("Date: ",date()),rdm)
  rdm <- sub("Tested Commit: .*",paste0("Tested commit: ",system("git log -1",intern=TRUE)[[1]]),rdm)
  if (modelinerror) {
   rdm <- sub(paste0(model," is in .*"),paste0(model, " is in ERROR"),rdm)
  } else {
   rdm <- sub(paste0(model," is in .*"),paste0(model, " is in PRODUCTION"),rdm)
  }
  
  dir.create("modeltesttmp")
  writeLines(rdm,"modeltesttmp/README.md")
  sendmail(gitdir,paste0(getwd(),"/modeltesttmp/README.md"))
}

