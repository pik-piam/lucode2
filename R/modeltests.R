#' modeltests
#'
#' Runs a group of tests for a specific model. A "config/scenario_config_AMT.csv" file
#' (relative) to the main folder has to exist in the model, describing the 
#' scenarios to be tested. Also, a gitlab repository is needed to push 
#' the generated README.md for documentation and automated reporting
#' of test results.
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
  argv <- "config/scenario_config_AMT.csv"
  slurmConfig <- "--qos=priority --nodes=1 --tasks-per-node=12"
  source("start.R",local=TRUE)

  out<-list()
  modelinerror = FALSE

  runcode<- paste0("-AMT-.*.202[1-9]-[0-1][0-9]-",format(Sys.Date(),"%d"))
  repeat {
    if(!any(grepl(runcode,system(paste0("squeue -u ",user," -h -o '%i %q %T %C %M %j %V %L %e %Z'"),intern=TRUE) ))) break
  }

  runcode2 <- sub("-AMT-\\.\\*\\.","",runcode)

  # Did all the runs start?
  runs <- system(paste0("find ",dir,"/output -name 'full.gms'| grep ",runcode2),intern=TRUE)
  amtcsv<-read.csv2(paste0(dir,"/config/scenario_config_AMT.csv"))
  start<-NULL
  amtcsv <- filter(amtcsv,start==1)
  out["Runs"]<-"yes"
  if (length(runs) < length(amtcsv[,"start"]) ) out["Runs"]<-"Some runs did not start"

  # Which runs finished properly?
  mifs <- system(paste0("find ",dir,"/output -name ",model,"*mif"),intern=TRUE)
  mifs <- grep(runcode,mifs,value=TRUE)
  mifs <- grep("_withoutPlus",mifs,invert = TRUE, value = TRUE)
  out["mifs"] <- "yes"
  if (length(mifs) < length(amtcsv[,"start"]) ) out["mifs"]<-"Some runs did not write a reporting file"

  a <- read.quitte(mifs)
  out[["iamCheck"]] <- iamCheck(a,model)


  rdm<-readLines(paste0(gitdir,"/README_temp.md"))
  rdm <- sub("Date: .*",paste0("Date: ",date(),". Direct link to this report: see top of email"),rdm)
  rdm <- sub("Tested Commit: .*",paste0("Tested commit: ",system("git log -1",intern=TRUE)[[1]]),rdm)
  
  if (modelinerror) {
   rdm <- sub(paste0(model," is in .*"),paste0(model, " is in ERROR"),rdm)
  } else {
   rdm <- sub(paste0(model," is in .*"),paste0(model, " is in PRODUCTION"),rdm)
  }
  rdm <- sub("Did all the runs start: .*",paste0("Did all the runs start: ",date(),out["Runs"]),rdm)

  rdm <- sub("Did all the runs converge: .*",paste0("Did all the runs converge: ",date(),out["Conv"]),rdm)

  rdm <- sub("Did all the runs report: .*",paste0("Did all the runs report: ",date(),out["mifs"]),rdm)
  
  writeLines(rdm,paste0(tempdir(),"/README.md"))
  sendmail(gitdir,paste0(tempdir(),"/README.md"),"Test Results")
}

