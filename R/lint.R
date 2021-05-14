#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' Returns a named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.
#'
#' @param filesToLint A character vector of paths to files that should be checked by the linter.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @examples
#' lucode2::lint()
#' @export
lint <- function(filesToLint = getFilesToLint()) {
  linters <- lintr::with_defaults(line_length_linter = lintr::line_length_linter(120),
                                  object_name_linter = lintr::object_name_linter(styles = "camelCase"),
                                  cyclocomp_linter = NULL)
  return(sapply(filesToLint, lintr::lint, linters = linters))
}
