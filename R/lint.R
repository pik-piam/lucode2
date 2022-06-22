#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' For files in the vignettes and tests folder less strict rules are applied, e.g. using ::: usually
#' leads to a linter warning, but not in vignettes/tests. Which linter rules are used depends on
#' ".lintr" config files. \code{\link{buildLibrary}} creates lintr config files that use \code{\link{lintrRules}}.
#'
#' @param files A character vector of paths to files that should be checked by the linter. If set to "."
#' the whole package is linted.
#' @return A named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.

#' @author Pascal Führlich
#' @seealso \code{\link{lintrRules}}, \code{\link{getFilesToLint}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @importFrom lintr lint_package with_defaults absolute_path_linter line_length_linter object_name_linter
#' todo_comment_linter undesirable_function_linter cyclocomp_linter default_undesirable_functions
#' undesirable_operator_linter default_undesirable_operators T_and_F_symbol_linter
#' @examples
#' lucode2::lint()
#' @export
lint <- function(files = getFilesToLint()) {
  if (identical(files, ".")) {
    return(lint_package())
  }

  files <- normalizePath(files)

  linterWarnings <- lapply(files, function(aFile) {
    message('Running lintr::lint("', aFile, '")')
    return(lintr::lint(aFile))
  })

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
