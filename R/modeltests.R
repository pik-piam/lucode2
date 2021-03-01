#' modeltests
#' 
#' Runs a group of tests for a specific model.
#' 
#' 
#' @param dir Path to the folder where the model is found
#' @param model Model name
#' 
#' 
#' @author Anastasis Giannousakis
#' @seealso \code{\link{package2readme}}
#' @importFrom yaml read_yaml write_yaml
#' @examples
#' 
#' \dontrun{modeltests()}
#' @export 
modeltests<-function(dir=".", model=NULL){

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
    return(s);
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
  
  if (is.null(model)) stop("Model cannot be NULL")

  source("start.R")

  runcode<- paste0("-AMT-.*.202[1-9]-[0-1][0-9]-",format(Sys.Date(),"%d"))
  repeat {
    print("in")
    if(!any(grepl(runcode,system("squeue -u giannou -h -o '%i %q %T %C %M %j %V %L %e %Z'",intern=TRUE) ))) break
  }
print("out")
  #  mifs <- system("find /p/projects/remind/modeltests/ -name REMIND*mif",intern=TRUE)
#  a <- quitte::read.quitte()
}
