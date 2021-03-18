#' Update package repository
#' 
#' Function to update an package repository. Run this function on a folder which contains
#' packages sources as subfolders. Packages should be either managed via svn or git
#' in order to be updated properly.
#' To add a new package to the repo just checkout/clone it into this folder. The
#' function will automatically detect the new package and add it.
#' 
#' 
#' @param path Path to the repository
#' @param check Boolean deciding whether package must have been checked or not 
#' in order to be distributed
#' @param force_rebuild Option to rebuild all packages from source
#' @param clean Option to clean repos before updating/pulling to avoid merge conflicts
#' @param pidfile file name of a "process id" file containing the process id of the current 
#' process. If set this will make sure that the function is not run twice at the same time. 
#' If set to NULL this will be ignored. The OS needs to provide a "ps" command in order to
#' have this working.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @export

updateRepo <- function(path=".", check=TRUE, force_rebuild=FALSE, clean=FALSE, pidfile=NULL) {
  cat(date(),"\n")
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  if(!is.null(pidfile)) {
    if(file.exists(pidfile)) {
      pid <- readLines(pidfile)
      if(any(grepl(paste0("^ *",pid," "),system("ps -A", intern = TRUE)))) {
        message("Process already running.")
        return()
      }
    } 
    writeLines(as.character(Sys.getpid()),pidfile)
  }
  if(file.exists("/webservice")){
    Sys.setenv("RSTUDIO_PANDOC"="/usr/local/bin/pandoc")
  }
  if(dir.exists(".svn")) {
    if(clean) system("svn revert -Rq .")
    system("svn update -q")
  }
  ap <- suppressWarnings(available.packages(paste0("file:",getwd()), filters = "duplicates"))
  apCRAN <- suppressWarnings(available.packages("https://cloud.r-project.org/src/contrib", filters = "duplicates"))
  
  dirs <- grep("^\\.",list.dirs(recursive = FALSE,full.names = FALSE), value=TRUE, invert=TRUE)
  nchar <- max(nchar(dirs))
  update_PACKAGES <- FALSE
  for(d in dirs) {
    if (d=="Archive") next
    setwd(d)
    if(dir.exists(".svn")) {
      if(clean) system("svn revert -Rq .; svn update -q", wait = FALSE)
      else system("svn update -q", wait=FALSE)
    }
    if(dir.exists(".git")) {
      if(clean) system("git reset --hard HEAD -q; git clean -fxq; git pull -q", wait = FALSE)
      else system("git pull -q", wait = FALSE)
    }  
    setwd("..")
  }
  dirs <- dirs[order(tolower(dirs))]
  for(d in dirs) {
    if (d=="Archive") next
    fd <- format(d,width=nchar)
    curversion <- tryCatch(ap[d,"Version"],error=function(e)return(0))
    setwd(d)
    vkey <- validkey()
    pattern <- paste0("^",d,"_(.*)\\.tar\\.gz")
    build_version <- max(as.numeric_version(sub(pattern,"\\1",dir("..",pattern=pattern))))
    if(length(build_version)==0) build_version <- as.numeric_version(0)
    
    if(as.numeric_version(curversion) < as.numeric_version(vkey$version) | force_rebuild) {
      if(vkey$valid | !check | force_rebuild) {
        error <- NULL
        if(vkey$roxygen) error <- try(devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette')))
        if(!("try-error" %in% class(error))) error <- try(devtools::build())
        if("try-error" %in% class(error)) {
          message(".:: ",fd," ",curversion," -> ",vkey$version," build failed ::.")
          if(dir.exists(".git")) system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
        } else {
          update_PACKAGES <- TRUE
          message(".:: ",fd," ",curversion," -> ",vkey$version," build success ::.")
        }
      } else {
        message(".:: ",fd," ",curversion," -> ",vkey$version," invalid commit ::.")
        if(dir.exists(".git")) system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
      }
    } else if(as.numeric_version(curversion) < build_version) {
      message(".:: ",fd," ",curversion," -> not build as newer version (",build_version,") is already part of the repo ::.")
    } else if(as.numeric_version(curversion)==as.numeric_version(vkey$version) && 
              as.numeric_version(curversion) > build_version){ 
      error <- try(devtools::build())
      if("try-error" %in% class(error)) {
        message(".:: ",fd," ",curversion," -> package build failed ::.")
        if(dir.exists(".git")) system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
      } else {
        message(".:: ",fd," ",curversion," -> package build success ::.")
      }
    } else {
      craninfo <- ""
      if(d %in% rownames(apCRAN)) {
        cranversion <- apCRAN[d,"Version"]
        if(as.numeric_version(cranversion) > as.numeric_version(curversion)) {
          warning("Package version of package \"",d,"\" is newer on CRAN (",cranversion,
                  ") compared to PIK-CRAN (",curversion,")! Check version on CRAN immediatly!")
          craninfo <- paste0(" .::WARNING! CRAN: ",apCRAN[d,"Version"]," !WARNING::.")
        } else {
          craninfo <- paste0(" .::CRAN: ",apCRAN[d,"Version"],"::.")
        }
      }
      message(".:: ",fd," ",format(curversion, width=10)," ok ::.", craninfo)
    }
    setwd("..")
  }
  if (update_PACKAGES) {
    tools::write_PACKAGES(unpacked = TRUE)
    for (d in dirs) {
      if (d=="Archive") next
      targz <- grep(paste0("^",d,".*.tar.gz"),dir(),value=TRUE)
      if (length(targz)>1) {
        newest<-max(numeric_version(sub("^.*_","",sub(".tar.gz$","",targz))))
        targz <- targz[-grep(newest,targz)]
        if (file.exists(paste0("Archive/",d))) {
          file.rename(targz,paste0("Archive/",d,"/",targz))
        } else {
          file.create(paste0("Archive/",d))
          file.rename(targz,paste0("Archive/",d,"/",targz))
        }
      }
    }
  } 
  if (!is.null(pidfile)) file.remove(pidfile)
  message("done.")
}
