#' lint
#'
#' Check the given files for linter warnings using lintr::lint.
#'
#' For files in the tests folder less strict rules are applied, e.g. using ::: usually leads to a linter warning, but
#' not in tests.
#'
#' @param files A character vector of paths to files that should be checked by the linter. If set to "."
#' the whole package is linted.
#' @return A named list, where the names are the paths to the linted files and the values are lists containing the
#' linter warnings.

#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}, \code{\link{autoFormat}}, \code{\link[lintr]{lint}}
#' @importFrom lintr lint_package with_defaults absolute_path_linter line_length_linter object_name_linter
#' todo_comment_linter undesirable_function_linter cyclocomp_linter default_undesirable_functions
#' undesirable_operator_linter T_and_F_symbol_linter
#' @examples
#' lucode2::lint()
#' @export
lint <- function(files = getFilesToLint()) {
  # names = deprecated functions, values = replacement hint
  deprecatedFunctions <- list(
    `on.exit` = "use withr::defer",
    fulldim = "use magclass::getItems()",
    getRegionList = "use magclass::getItems()",
    getRegions = "use magclass::getItems()",
    `getRegions<-` = "use magclass::getItems()"
  )

  # in tests undesirable functions (excepted deprecated ones) and undesirable operators are ok
  linterArgsForTests <- list(
    absolute_path_linter = absolute_path_linter(),
    line_length_linter = line_length_linter(120),
    object_name_linter = object_name_linter(styles = "camelCase"),
    todo_comment_linter = todo_comment_linter(),
    undesirable_function_linter = undesirable_function_linter(deprecatedFunctions),
    cyclocomp_linter = cyclocomp_linter(25),
    T_and_F_symbol_linter = T_and_F_symbol_linter
  )
  lintersForTests <- do.call(with_defaults, linterArgsForTests)

  linterArgs <- linterArgsForTests
  linterArgs$undesirable_function_linter <- # nolint
    undesirable_function_linter(c(default_undesirable_functions, deprecatedFunctions))
  linterArgs$undesirable_operator_linter <- undesirable_operator_linter() # nolint
  linters <- do.call(with_defaults, linterArgs)

  if (identical(files, ".")) {
    files <- list.files(path = c("tests", "R", "inst"), pattern = "\\.R(md|nw)?$", recursive = TRUE, full.names = TRUE)
  }

  files <- normalizePath(files)
  # use lintersForTests instead of linters if file is in testFiles
  testFiles <- normalizePath(list.files(path = "tests", recursive = TRUE, full.names = TRUE))

  linterWarnings <- lapply(files, function(aFile) {
    message('Running lucode2::lint("', aFile, '")')
    return(lintr::lint(aFile, linters = if (aFile %in% testFiles) lintersForTests else linters))
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
