#' package2readme
#' 
#' Creates a README.md for a R package. 
#' 
#' @param package either the path to the main folder of a package (containing a DESCRIPTION file)
#' or the name of the package
#' @author Jan Philipp Dietrich
#' @importFrom desc desc
#' @importFrom utils citation vignette
#' @importFrom usethis git_remotes
#' @examples
#' 
#' package2readme("lucode2")
#' @export
package2readme <- function(package=".") {
  if(file.exists(paste0(package,"/DESCRIPTION"))) {
    d <- desc(file=paste0(package,"/DESCRIPTION"))
    folder <- package
  } else {
    d <- desc(package=package)
    folder <- NULL
  }
  
  getGitHubRepo <- function(d,folder) {
    .harmonize <- function(x) {
      return(sub("\\.git$","",sub(":","/",sub("^[^@]*@","",sub("https://","",x)))))
    }
    z <- grep("github",d$get_urls(),value=TRUE)
    if(length(z)>0) return(.harmonize(z[1]))
    if(is.null(folder)) return(NULL)
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(folder)
    out <- try(usethis::git_remotes(), silent=TRUE)
    if("try-error" %in% class(out)) return(NULL)
    return(.harmonize(out))
  }
  
  withTravis <- function(folder) {
    travisfile <- paste0(folder,"/.travis.yml")
    if(!is.null(folder) && file.exists(travisfile)) return(TRUE)
    return(FALSE)
  }
  
  withGithubActions <- function(folder) {
    ghactionsfile <- Sys.glob(paste0(folder,"/.github/workflows/*.y*ml"))
    if(length(ghactionsfile)>0) return(TRUE)
    return(FALSE)
  }
  
  withCodecov <- function(folder) {
    if(withTravis(folder)) {
      travisfile <- paste0(folder,"/.travis.yml")
      if(file.exists(travisfile)) {
        tmp <- readLines(travisfile)
        if(any(grepl("codecov",tmp))) return(TRUE)
      }
    }
    if(withGithubActions(folder)) {
      ghafile <- Sys.glob(paste0(folder,"/.github/workflows/*.y*ml"))
      if(length(ghafile)>0) {
        for(f in ghafile) {
          tmp <- readLines(f)
          if(any(grepl("codecov",tmp))) return(TRUE) 
        }
      }
    }
    return(FALSE)
  }
  
  fillTravis <- function(d,folder){
    if(!withTravis(folder)) return("")
    pkg <- d$get("Package")
    z <- getGitHubRepo(d,folder)
    if(is.null(z)) return("")
    path <- sub("^[^/]*","",z)
    out <- paste0("[![Travis build status](https://travis-ci.com", 
                  path, ".svg?branch=master)](https://travis-ci.com",
                  path, ")")
    return(out)
  }
  
  fillGithubActions <- function(d,folder){
    if(!withGithubActions(folder)) return("")
    pkg <- d$get("Package")
    z <- getGitHubRepo(d,folder)
    if(is.null(z)) return("")
    out <- paste0("[![R build status](https://",
                  z, "/workflows/check/badge.svg)](https://",
                  z, "/actions)")
    return(out)
  }
  
  fillCRAN <- function(d){
    pkg <- d$get("Package")
    out <- paste0("[![CRAN status](https://www.r-pkg.org/badges/version/", pkg,
                  ")](https://cran.r-project.org/package=", pkg,")")
    return(out)
  }
  
  fillZenodo <- function(d) {
    z <- grep("zenodo",d$get_urls(),value=TRUE)
    if(length(z)==0) return("")
    doi <- strsplit(z,"doi.org/",fixed=TRUE)[[1]][2]
    out <- paste0("[![DOI](https://zenodo.org/badge/DOI/",doi,
                  ".svg)](https://doi.org/",doi,")")
    return(out)
  }
  
  fillCodecov <- function(d,folder) {
    if(!withCodecov(folder)) return("")
    z <- getGitHubRepo(d,folder)
    if(is.null(z)) return("")
    z <- sub("^[^/]*","",z)
    out <- paste0("[![codecov](https://codecov.io/gh",z,
                  "/branch/master/graph/badge.svg)](https://codecov.io/gh",z,")")
    return(out)
  }
  
  
  fillCite <- function(d) {
    out <- c("\nTo cite package **",d$get("Package"),"** in publications use:\n\n",
             format(citation(package=d$get("Package")),style="text"),
             "\n\nA BibTeX entry for LaTeX users is\n\n ```latex\n",
             format(citation(package=d$get("Package")),style="bibtex"),"\n```")
    return(paste(out,collapse=""))
  }
  
  fillVignette <- function(d,folder) {
    if(is.null(folder)) {
      v <- vignette(package=d$get("Package"))$results
    } else {
      v <- matrix(nrow=0,ncol=2,dimnames=list(NULL,c("Item","Title")))
      path <- paste0(folder,"/vignettes/")
      vig <- dir(path,pattern = "*.Rmd")
      for(i in vig) {
        tmp <- readLines(paste0(path,i),n = 5)
        tmp <- c(Item = sub(".Rmd","",i,fixed=TRUE),
                 Title= gsub("title: \"(.*)\"$","\\1",grep("title:",tmp, value=TRUE)))
        v <- rbind(v,tmp)
      }
    }
    if(dim(v)[1]==0) return("")
    else if(dim(v)[1]==1) {
      vtext <- "a vignette"
      vtext2 <- "it"
    } else {
      vtext <- "vignettes"
      vtext2 <- "them"
    }
    tmp <- paste0("vignette(\"",v[,"Item"],"\")")
    tmp <- format(tmp,width = max(nchar(tmp)))
    vig <- paste0(tmp," # ", sub("(source, html)","",v[,"Title"],fixed=TRUE), 
                  collapse="\n")
    out <- c("\n## Tutorial\n\n",
             "The package comes with ",vtext," describing the basic functionality ",
             "of the package and how to use it. You can load ",vtext2," with the following ",
             "command (the package needs to be installed):\n\n",
             "```r\n",
             vig,
             "\n```\n")
    return(paste(out,collapse=""))
  }
  
  fillTemplate <- function(x,fill) {
    for(what in names(fill)) {
      x <- gsub(paste0("[:",what,":]"),fill[[what]],x,fixed=TRUE)
    }
    return(x)
  }
  
  template <- readLines(system.file("extdata","README_template.md",package = "lucode2"))
 
  fill <- list(title         = d$get("Title"),
               package       = d$get("Package"),
               description   = d$get("Description"),
               version       = d$get("Version"),
               maintainer    = d$get_maintainer(),
               cran          = fillCRAN(d),
               zenodo        = fillZenodo(d),
               travis        = fillTravis(d,folder),
               githubactions = fillGithubActions(d,folder),
               codecov       = fillCodecov(d,folder),
               cite          = fillCite(d),
               vignette      = fillVignette(d,folder))
  
  out <- fillTemplate(template, fill)

  if(!is.null(folder)) {
    readmefile <- paste0(folder,"/README.md")
    if(file.exists(readmefile)) message("Updated README.md file")
    else message("Added README.md file")
    writeLines(out,readmefile)
  } else {
    message(paste(out,collapse="\n"))
  }
  invisible(out)
}
