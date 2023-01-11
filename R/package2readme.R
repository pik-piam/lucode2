#' package2readme
#'
#' Creates a README.md for a R package.
#'
#' @param package either the path to the main folder of a package (containing a DESCRIPTION file)
#' or the name of the package
#' @param add a character vector with additions to the README file. Each element of the vector can be either
#' 1) a line of markdown code, 2) a path to a markdown file, or 3) a path to a Rmarkdown file
#' @param logo a character string for a path to a logo file used in the title of the README file
#' @author Jan Philipp Dietrich
#' @importFrom desc desc
#' @importFrom utils citation vignette packageDescription
#' @importFrom usethis git_remotes with_project
#' @importFrom withr local_options local_tempfile local_connection
#' @importFrom tools file_ext
#' @examples
#' package2readme("lucode2")
#' @export
package2readme <- function(package = ".", add = NULL, logo = NULL) { # nolint
  if (file.exists(file.path(package, "DESCRIPTION"))) {
    d <- desc(file = file.path(package, "DESCRIPTION"))
    folder <- package
  } else {
    d <- desc(package = package)
    folder <- NULL
  }

  getGitHubRepo <- function(d, folder) {
    .harmonize <- function(x) {
      return(sub("\\.git$", "", sub(":", "/", sub("^[^@]*@", "", sub("https://", "", x)))))
    }
    z <- grep("github", d$get_urls(), value = TRUE)
    if (length(z) > 0) {
      return(.harmonize(z[1]))
    }
    if (is.null(folder)) {
      return(NULL)
    }
    with_project(folder, {
      out <- try(git_remotes(), silent = TRUE)
    }, quiet = TRUE)
    if ("try-error" %in% class(out)) {
      return(NULL)
    }
    out <- ifelse("origin" %in% names(out), out[["origin"]], out[[1]])
    return(.harmonize(out))
  }

  withGithubActions <- function(folder) {
    ghactionsfile <- Sys.glob(file.path(folder, ".github", "workflows", "*.y*ml"))
    if (length(ghactionsfile) > 0) return(TRUE)
    return(FALSE)
  }

  withCodecov <- function(folder) {
    if (withGithubActions(folder)) {
      ghafile <- Sys.glob(file.path(folder, ".github", "workflows", "*.y*ml"))
      if (length(ghafile) > 0) {
        for (f in ghafile) {
          tmp <- readLines(f)
          if (any(grepl("codecov", tmp))) return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  fillGithubActions <- function(d, folder) {
    if (!withGithubActions(folder)) return("")
    z <- getGitHubRepo(d, folder)
    if (is.null(z)) return("")
    out <- paste0("[![R build status](https://",
                  z, "/workflows/check/badge.svg)](https://",
                  z, "/actions)")
    return(out)
  }

  fillCRAN <- function(d) {
    pkg <- d$get("Package")
    out <- paste0("[![CRAN status](https://www.r-pkg.org/badges/version/", pkg,
                  ")](https://cran.r-project.org/package=", pkg, ")")
    return(out)
  }

  fillZenodo <- function(d) {
    z <- grep("zenodo", d$get_urls(), value = TRUE)
    if (length(z) == 0) return("")
    doi <- strsplit(z, "doi.org/", fixed = TRUE)[[1]][2]
    out <- paste0("[![DOI](https://zenodo.org/badge/DOI/", doi,
                  ".svg)](https://doi.org/", doi, ")")
    return(out)
  }

  fillCodecov <- function(d, folder) {
    if (!withCodecov(folder)) return("")
    z <- getGitHubRepo(d, folder)
    if (is.null(z)) return("")
    z <- sub("^[^/]*", "", z)
    out <- paste0("[![codecov](https://codecov.io/gh", z,
                  "/branch/master/graph/badge.svg)](https://app.codecov.io/gh", z, ")")
    return(out)
  }

  loadFromLucode2 <- function(filename, d) {
    if (d$get("Package") == "lucode2" && !is.null(folder)) {
      # can't load the file from the package when we are working on ourselves
      # load it directly instead
      out <- file.path(folder, "inst", "extdata", filename)
      if (!file.exists(out)) {
        # for binary packages, the inst folder does not exist
        out <- file.path(folder, "extdata", filename)
      }
      return(out)
    } else {
      return(system.file("extdata", filename, package = "lucode2"))
    }
  }

  fillRUniverse <- function(nameOfPackage, d) {
    if (any(grepl(nameOfPackage, piamPackages(), fixed = TRUE))) {
      return(paste0("[![r-universe](https://pik-piam.r-universe.dev/badges/", nameOfPackage,
                    ")](https://pik-piam.r-universe.dev/ui#builds)"))
    } else {
      return("")
    }
  }

  fillCite <- function(d, folder) {
    # the format function wraps lines according to the width option, set explicitly so it is always the same
    local_options(list(width = 664)) # 664 * 0.75 is <= 500; will get deparse cutoff warning if > 500

    # Get the citation object.
    ci <- NULL
    # If `folder` is set, try to get citation from this project folder.
    if (!is.null(folder)) {
      try({
        projectPath <- normalizePath(folder, mustWork = TRUE, winslash = "/")
        ci <- citationDoi(packageDescription(
          basename(projectPath),
          lib.loc = dirname(projectPath)))
      })
    }
    # If previous code did not work or folder is not set, get citation from installed package.
    if (is.null(ci)) {
      ci <- citation(package = d$get("Package"))
    }

    out <- c("\nTo cite package **", d$get("Package"), "** in publications use:\n\n",
             format(ci, style = "text"),
             "\n\nA BibTeX entry for LaTeX users is\n\n ```latex\n",
             format(ci, style = "bibtex"), "\n```")
    return(paste(out, collapse = ""))
  }

  fillVignette <- function(d, folder) {
    if (is.null(folder)) {
      v <- vignette(package = d$get("Package"))$results
    } else {
      v <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("Item", "Title")))
      path <- file.path(folder, "vignettes")
      vig <- list.files(path, pattern = ".*\\.Rmd$")
      for (i in vig) {
        tmp <- readLines(file.path(path, i), n = 5)
        tmp <- c(Item = sub(".Rmd", "", i, fixed = TRUE),
                 Title = gsub("title: \"(.*)\"$", "\\1", grep("title:", tmp, value = TRUE)))
        v <- rbind(v, tmp)
      }
    }
    if (dim(v)[1] == 0) return("")
    else if (dim(v)[1] == 1) {
      vtext <- "a vignette"
      vtext2 <- "it"
    } else {
      vtext <- "vignettes"
      vtext2 <- "them"
    }
    tmp <- paste0("vignette(\"", v[, "Item"], "\")")
    tmp <- format(tmp, width = max(nchar(tmp)))
    vig <- paste0(tmp, " # ", sub("(source, html)", "", v[, "Title"], fixed = TRUE),
                  collapse = "\n")
    out <- c("\n## Tutorial\n\n",
             "The package comes with ", vtext, " describing the basic functionality ",
             "of the package and how to use it. You can load ", vtext2, " with the following ",
             "command (the package needs to be installed):\n\n",
             "```r\n",
             vig,
             "\n```\n")
    return(paste(out, collapse = ""))
  }

  fillAdditions <- function(add, folder) {
    if (is.null(add)) return("")
    out <- NULL
    for (a in add) {
      if (file.exists(file.path(folder, a))) a <- file.path(folder, a)
      if (file.exists(a)) {
        fileType <- file_ext(a)
        if (tolower(fileType) == "rmd") {
          checkRequiredPackages("knitr")
          tmpFile <- local_tempfile()
          knitr::knit(a, output = tmpFile)
          a <- readLines(tmpFile)
        } else if (tolower(fileType) == "md") {
          a <- readLines(a)
        } else {
          stop("Unsupported file type \"", fileType, "\"")
        }
      }
      out <- c(out, a)
    }
    out <- paste(out, collapse = "\n")
    return(out)
  }

  fillTemplate <- function(x, fill) {
    for (what in names(fill)) {
      if (is.null(fill[[what]])) fill[[what]] <- ""
      x <- gsub(paste0("[:", what, ":]"), fill[[what]], x, fixed = TRUE)
    }
    return(x)
  }

  getTitle <- function(x, logo) {
    if (!is.null(logo)) {
      x <- paste0(x,
                  " <a href=''><img src='",
                  logo,
                  "' align='right' height='139' /></a>")
    }
    return(x)
  }

  fill <- list(title         = getTitle(d$get("Title"), logo),
               package       = d$get("Package"),
               description   = d$get("Description"),
               additions     = fillAdditions(add, folder),
               version       = d$get("Version"),
               maintainer    = d$get_maintainer(),
               cran          = fillCRAN(d),
               zenodo        = fillZenodo(d),
               githubactions = fillGithubActions(d, folder),
               codecov       = fillCodecov(d, folder),
               runiverse     = fillRUniverse(d$get("Package"), d),
               cite          = fillCite(d, folder),
               vignette      = fillVignette(d, folder))

  template <- readLines(loadFromLucode2("README_template.md", d))
  out <- fillTemplate(template, fill)

  if (!is.null(folder)) {
    readmefile <- file.path(folder, "README.md")
    if (file.exists(readmefile)) {
      message("Updated README.md file")
    } else {
      message("Added README.md file")
    }
    # enc2utf8 re-encodes out as utf8, encoding = "" and useBytes = TRUE prevent automatic re-encoding
    writeLines(enc2utf8(out), local_connection(file(readmefile, "w+", encoding = "")), useBytes = TRUE)
  } else {
    message(paste(out, collapse = "\n"))
  }
  invisible(out)
}
