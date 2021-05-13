#' buildLibrary
#' 
#' Builds R libraries. Includes checks for consistency.
#' 
#' This function is designed to help building R libraries. It performs the
#' following steps: \itemize{ \item Version: Determination
#' of a new version number (Can also be defined by the user). \item Date:
#' Determination of a the date of the build (Can also be defined by the user).
#' \item R check: Check whether the library is consistent and can be built.
#' \item Package building Builds the .zip and .tar.gz packages under windows.
#' Under linux, only the .tar.gz package is built. } The commit has to be performed by the user still.
#' 
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param gitpush If a git commit should happen automatically
#' @param commitmessage Your commit message
#' @param autoFormat set to TRUE to auto-format changed R files with styler
#' @param update_type Either an integer or character string:
#' 
#'   | **number**  | **string**    | **description**                          |
#'   |-------------|---------------|------------------------------------------|
#'   | 1           | `major`       | for major rewrite of the whole package   |
#'   | 2 (default) | `minor`       | for new features or improvements         |
#'   | 3           | `patch`       | for bugfixes and corrections             |
#'   | 4           | `development` | only for packages in development stage   |
#' 
#' @md
#' 
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch
#' @seealso \code{\link{package2readme}}
#' @importFrom citation package2zenodo
#' @importFrom yaml read_yaml write_yaml
#' @importFrom styler style_pkg style_file
#' @importFrom lintr lint
#' @importFrom dplyr %>%
#' @examples
#' 
#' \dontrun{buildLibrary()}
#' @export 
buildLibrary<-function(lib=".",cran=TRUE, update_type=NULL,gitpush=FALSE,commitmessage=NULL, autoFormat = FALSE){

  get_line <- function(){
    # gets characters (line) from the terminal or from a connection
    # and returns it
    if(interactive()){
      s <- readline()
    } else {
      con <- file("stdin")
      s <- readLines(con, 1, warn=FALSE)
      on.exit(close(con))
    }
    return(s)
  }
  
  askYesNo <- function(question) {
    cat(paste(question,"(yes/no)"))
    s <- get_line()
    if(!(tolower(s) %in% c("y","yes"))) return(FALSE)
    return(TRUE)
  }  
    
  didYouPull <- function() {
    if(!askYesNo("Is your repository up-to-date? Did you pull immediately before running this check?")){
      stop("Please update your repository first, before you proceed!")  
    }
  }
  didYouPull()
  
  thisdir<-getwd()
  if(lib!=".") setwd(lib)
  on.exit(setwd(thisdir))
  
  ####################################################################
  #Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$",base::list.dirs(full.names = FALSE,recursive = FALSE),value=TRUE)
  unlink(rcheckfolders,recursive=TRUE)
  
  ####################################################################
  #Check if roxygen is used and run roxygenize if required
  ################################################################### 
  descfile<-readLines("DESCRIPTION")
  if(any(grepl("RoxygenNote",descfile))) {
    devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette'))
    roxygen <- TRUE
  } else {
    roxygen <- FALSE
  }
  
  ############################################################
  # identify changed files
  ############################################################
  # untracked, unstaged, staged files
  recentlyChanged <- system("git status --porcelain", intern = TRUE) %>%  # get git status in parsable form
    (function(x) grep("^ ?D", x, value = TRUE, invert = TRUE)) %>%  # filter out deleted files
    substring(4)  # trim info on modified/added/staged

  # files changed in any non-merge commit authored by current git user within the last 30 days
  previouslyChanged <- system(paste0('git log --name-only --format=format:"" --no-merges --since="30 days"',
                                          ' --author="', system("git config user.email", intern = TRUE), '"'),
                                   intern = TRUE)

  # remove duplicate files, keep only .R .Rmd .Rnw files
  changedRFiles <- grep(pattern = "\\.R(md|nw)?$", x = unique(c(recentlyChanged, previouslyChanged)), value = TRUE)
  if (length(changedRFiles) > 0) {
    # prepend git root path, so we get absolute paths
    changedRFiles <- paste0(system("git rev-parse --show-toplevel", intern = TRUE), "/", changedRFiles)
  }

  ############################################################
  # auto-format
  ############################################################
  if (autoFormat) {
    sapply(changedRFiles, style_file)
  } else {
    cat("Skipping auto-formatting (to enable it run buildLibrary with autoFormat = TRUE)\n")
  }

  ############################################################
  # linter
  ############################################################
  linterWarningFiles <- names(sapply(changedFiles, lint))
  if (length(linterWarningFiles) > 0) {
    cat(paste0("Run lintr::lint(\"", linterWarningFiles, "\") to see linter warnings.", collapse = "\n"))
    cat("\n")
    stop(paste("See above for linter warnings. You need to address these before submission!",
               "You can also run buildLibrary with autoFormat = TRUE to fix some warnings.",
               "In exceptional cases disabling the linter might be okay, ?lintr::exclude describes how to do that."))
  }

  ############################################################
  #check the library
  ############################################################
  
  if(!file.exists(".buildlibrary")) {
    # if not yet available, add .buildlibrary and add to .Rbuildignore
    cfg <- list(ValidationKey=0, 
                AutocreateReadme=TRUE,
                AcceptedWarnings=c("Warning: package '.*' was built under R version",
                                   "Warning: namespace '.*' is not available and has been replaced"), 
                AcceptedNotes=NULL)
    write_yaml(cfg,".buildlibrary")
    message("Created .buildlibrary config file and added it to .Rbuildignore. Please add it to your next commit!")
    if(file.exists(".Rbuildignore")) {
      a <- c(readLines(".Rbuildignore") , "^\\.buildlibrary$")
      if(anyDuplicated(a)) a <- a[!duplicated(a)]
    } else {
      a <- "^\\.buildlibrary$"
    }
    writeLines(a,".Rbuildignore")
  }
  
  cfg <- read_yaml(".buildlibrary")
  
  if(is.null(cfg$AutocreateReadme)) cfg$AutocreateReadme <- TRUE
  if(is.null(cfg$UseGithubActions) && 
     askYesNo("Do you want to use GitHub Actions for package testing?")) cfg$UseGithubActions <- TRUE
  
  if(isTRUE(cfg$UseGithubActions)) {
    addGitHubActions(lib)
    # remove travis related parts
    travisfile <- paste0(lib,"/.travis.yml")
    if(file.exists(travisfile)) {
      file.remove(travisfile)
      rbuildignore <- paste0(lib,"/.Rbuildignore")
      if(file.exists(rbuildignore)) {
        a <- readLines(rbuildignore)
        writeLines(grep("travis",a,value=TRUE,invert=TRUE), rbuildignore)
      }
      testfolder <- paste0(lib,"/tests/testthat")
      if (!file.exists(testfolder)) dir.create(testfolder, recursive = TRUE)
      travistest <- paste0(lib,"/tests/testthat/test-travisCI.R")
      if (file.exists(travistest)) file.remove(travistest)
      if (length(dir(testfolder) == 0)) writeLines('skip("dummy test")', paste0(testfolder,"/test-dummy.R"))
    }
  }
  
  ck <- devtools::check(".",cran=cran)
  
  #Filter warnings and notes which are accepted
  for(aw in cfg$AcceptedWarnings) {
    ck$warnings <- grep(aw, ck$warnings, value=TRUE,invert=TRUE)
  }
  for(aw in cfg$AcceptedNotes) {
    ck$notes <- grep(aw, ck$notes, value=TRUE,invert=TRUE)
  }
  print(ck)
  
  if(length(ck$errors)>0) {
    stop("The package check showed errors. You need to fix these errors first before submission!")
  }  
  
  if(length(ck$warnings)>0) {
    stop("The package check showed warnings. You need to take care of these warnings first before submission!")
  }

  if(length(ck$notes)>0) {
    stop("The package check showed notes. You need to take care of these notes first before submission!")
  }
  
  ##########################################################
  #Check for version numbers
  ##########################################################
  #Version number in the man file
  #Version number in the description file
  descfile<-readLines("DESCRIPTION")
  descfile_version<-sub("[^(0-9)]*$","",sub("Version:[^(0-9)]*","",grep("Version",descfile,value=T),perl=T),perl=T)
  
  version <- descfile_version
  
  autoversion <- function(old_version, upt, defLengths=3) {
    old_version <- numeric_version(old_version)
    if(upt==0) return(old_version)
    for(i in 1:upt) if(is.na(old_version[1,i])) old_version[1,i] <- 0
    if(old_version[1,upt] == 0 & upt==4) old_version[1,upt] <- 9000
    old_version[1,upt] <- as.numeric(old_version[1,i]) + 1
    if(defLengths>upt) {
      for(i in (upt+1):defLengths) {
        old_version[1,i] <- 0
      }
    }
    old_version <- old_version[1,1:max(upt,defLengths)]
    return(old_version)
  }

  
  choose_module <- function(Rfolder,title="Package check successful! Please choose an update type") {
    update_type <- c("major revision (for major rewrite of the whole package)", 
                     "minor revision (for new features or improvements)", 
                     "patch (for bugfixes and corrections)", 
                     "only for packages in development stage",
                     "no version increment (only to use if version is already incremented!)")
    cat(title,":\n")
    cat(paste(c(1:(length(update_type)-1),0), update_type, sep=": " ),sep="\n")
    cat("\nNumber: ")
    identifier <- get_line()
    identifier <- as.numeric(strsplit(identifier,",")[[1]])
    if (any(!(identifier %in% (1:length(update_type)-1)))) stop("This choice (",identifier,") is not possible. Please type in a number between 0 and ",length(update_type)-1)
    return(identifier)
  }
  
  if (is.null(update_type)) {
    update_type <- choose_module(".")
  } else {
    # convert character update_type parameters to numbers
    update_type <- switch(as.character(update_type),
           'major' = 1,       '1' = 1,
           'minor' = 2,       '2' = 2,
           'patch' = 3,       '3' = 3,
           'development' = 4, '4' = 4,
           # default
           choose_module('.'))
  }
  version <- autoversion(version,update_type)
  
  #Change the version in descfile
  descfile[grep("Version",descfile)]<-sub(descfile_version,version,descfile[grep("Version",descfile)])
  
  ############################################################
  #Check for the date
  ############################################################
  if(any(grepl("Date:",descfile))) {
    descfile_date<-sub("[^(0-9)]*$","",sub("Date:[^(0-9)]*","",grep("Date:",descfile,value=T),perl=T),perl=T)
    date <- Sys.Date()
    #Change the date in descfile      
    descfile[grep("Date:",descfile)]<-sub(descfile_date,date,descfile[grep("Date:",descfile)])
  }
    
  ############################################################
  # Update validation key
  ############################################################
  if(cran) {
    cfg$ValidationKey <- as.character(validationkey(version,date))
  } else {
    cfg$ValidationKey <- as.character(0)
  }
  if(any(grepl("ValidationKey:",descfile))) {
    descfile <- descfile[!grepl("ValidationKey:",descfile)]
  } 
  
  ##################################################################
  # Write the modified description files, update metadata and readme
  ##################################################################
  writeLines(descfile,"DESCRIPTION")
  write_yaml(cfg,".buildlibrary")
  package2zenodo(".")
  if(isTRUE(cfg$AutocreateReadme)) package2readme(".")
  
  ############################################################
  # Verbosity for version information and git commands
  ############################################################
  
  if(update_type != 0){
    cat(paste0("* updating from version"), descfile_version, "to version", toString(version), "... OK\n")
    if(file.exists(".git")){
      if (gitpush) {
        system(paste0('git add . && git commit -m "',commitmessage,' and type ', update_type, ' upgrade" && git push'))
      } else {
        cat("* git repository detected... OK\n")
        cat("* command suggestions for updating your git repository:\n")
        cat(rep("=", options()$width), "\n", sep="")
        cat(paste0('adding and committing: $ git add . && git commit -m "type ', update_type, ' upgrade"\n'))
        cat(paste0("version tagging: $ git tag ", version, "\n"))
        cat("push commits to github: $ git push <remote> <branch>\n")
        cat("push new tag to github: $ git push --tags\n")
        cat(rep("=", options()$width), "\n", sep="")
      }
    }
  }
  cat("done")
}
