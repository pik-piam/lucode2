#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' Returns a named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.
#'
#' @param files A character vector of paths to files that should be checked by the linter. If set to "."
#' the whole package is linted.
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @importFrom lintr lint_package
#' @examples
#' lucode2::lint()
#' @export
lint <- function(files = getFilesToLint()) {
  # names = deprecated functions, values = replacement hint
  deprecatedFunctions <- list(
    fulldim = "use magclass::getItems()"
  )

  linters <- lintr::with_defaults(
    absolute_path_linter = lintr::absolute_path_linter(),
    line_length_linter = lintr::line_length_linter(120),
    nonportable_path_linter = lintr::nonportable_path_linter(),
    object_name_linter = lintr::object_name_linter(styles = "camelCase"),
    todo_comment_linter = lintr::todo_comment_linter(),
    undesirable_function_linter = lintr::undesirable_function_linter(c(lintr::default_undesirable_functions,
                                                                       deprecatedFunctions)),
    undesirable_operator_linter = lintr::undesirable_operator_linter()
  )

  if (identical(files, ".")) {
    return(lint_package(linters = linters))
  }

  linterWarnings <- lapply(files, function(aFile) {
    message('Running lucode2::lint("', aFile, '")')
    return(lintr::lint(aFile, linters = linters))
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
