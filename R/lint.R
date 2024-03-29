#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' For files in the vignettes and tests folder less strict rules are applied, e.g. using ::: usually
#' leads to a linter warning, but not in vignettes/tests. Which linter rules are used depends on
#' ".lintr" config files. \code{\link{check}} creates lintr config files that use \code{\link{lintrRules}}.
#'
#' @param files A character vector of paths to files that should be checked by the linter. If set to "."
#' the whole package is linted.
#' @return A named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.

#' @author Pascal Sauer
#' @seealso \code{\link{getFilesToLint}}, \code{\link{lintrRules}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @examples
#' \dontrun{
#' lucode2::lint()
#' }
#' @export
lint <- function(files = getFilesToLint()) {
  if (length(files) == 0) {
    return(invisible(list()))
  }

  if (identical(files, ".")) {
    files <- list.files(pattern = "[.]R(md)?$", full.names = TRUE, recursive = TRUE)
  }

  files <- normalizePath(files)

  withr::local_dir(dirname(files[[1]]))

  # create .lintr config files if they do not exist
  gitRoot <- suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = TRUE))
  if (dir.exists(gitRoot)) {
    withr::with_dir(gitRoot, {
      writeIfNonExistent(c("linters: lucode2::lintrRules()", 'encoding: "UTF-8"'),
                         ".lintr")
      writeIfNonExistent(c("linters: lucode2::lintrRules(allowUndesirable = TRUE)", 'encoding: "UTF-8"'),
                         file.path("tests", ".lintr", fsep = "/"))
      writeIfNonExistent(c("linters: lucode2::lintrRules(allowUndesirable = TRUE)", 'encoding: "UTF-8"'),
                         file.path("vignettes", ".lintr", fsep = "/"))
    })
  }

  # separate R session so already attached packages are not considered, and load_all does not affect current R session
  linterWarnings <- callr::r(function(files) {
    devtools::document(roclets = "namespace")
    devtools::load_all(quiet = TRUE)

    return(lapply(seq_along(files), function(i) {
      if (length(files) > 1) {
        message("[", i, "/", length(files), "] ", appendLF = FALSE)
      }
      message('Running lintr::lint("', files[[i]], '")')
      return(lintr::lint(files[[i]]))
    }))
  }, args = list(files = files), show = TRUE)

  # combine the results of multiple calls to lintr::lint, taken from lintr:::flatten_lints
  flattenLints <- function(x) {
    res <- list()
    itr <- 1L
    assignItem <- function(x) {
      if (inherits(x, "lint")) {
        res[[itr]] <<- x # nolint
        itr <<- itr + 1L # nolint
      } else if (is.list(x)) {
        lapply(x, assignItem)
      }
    }
    assignItem(x)
    return(structure(res, class = "lints"))
  }

  linterWarnings <- flattenLints(linterWarnings)
  class(linterWarnings) <- "lints"
  return(linterWarnings)
}

writeIfNonExistent <- function(fileText, filePath) {
  if (!dir.exists(dirname(filePath))) {
    return()
  }
  if (!file.exists(filePath)) {
    writeLines(fileText, filePath)
  }
  if (!paste0("^", filePath, "$") %in% readLines(".Rbuildignore")) {
    write(paste0("^", filePath, "$"), ".Rbuildignore", append = TRUE)
  }
}
