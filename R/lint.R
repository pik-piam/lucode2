#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' Returns a named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.
#'
#' @param filesToLint A character vector of paths to files that should be checked by the linter. If filesToLint = "."
#' the whole package is linted.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @importFrom lintr lint_package
#' @examples
#' lucode2::lint()
#' @export
lint <- function(filesToLint = getFilesToLint()) {
  linters <- lintr::with_defaults(line_length_linter = lintr::line_length_linter(120),
                                  object_name_linter = lintr::object_name_linter(styles = "camelCase"),
                                  cyclocomp_linter = NULL)
  if (identical(filesToLint, ".")) {
    return(lint_package(linters = linters))
  }
  linterWarnings <- sapply(filesToLint, lintr::lint, linters = linters)

  # required to combine the results of multiple calls to lintr::lint, taken from lintr:::flatten_lints
  .flattenLints <- function(x) {
    res <- list()
    itr <- 1L
    assignItem <- function(x) {
      if (inherits(x, "lint")) {
        res[[itr]] <<- x
        itr <<- itr + 1L
      }
      else if (is.list(x)) {
        lapply(x, assignItem)
      }
    }
    assignItem(x)
    return(structure(res, class = "lints"))
  }
  linterWarnings <- .flattenLints(linterWarnings)
  class(linterWarnings) <- "lints"
  return(linterWarnings)
  # TODO test
}
