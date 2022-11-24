#' lintrRules
#'
#' This function defines the rules to be used by the linter called lintr. \code{\link{check}} creates ".lintr"
#' config files that use this function.
#'
#' @param allowUndesirable If true it is okay to use undesirable operators (such as "<<-")
#' and undesirable (but not deprecated) functions (such as "setwd").
#'
#' @examples
#' \dontrun{lintr::lint_dir(linters = lintrRules())}
#'
#' @importFrom lintr linters_with_defaults absolute_path_linter line_length_linter object_name_linter
#' todo_comment_linter undesirable_function_linter cyclocomp_linter default_undesirable_functions
#' undesirable_operator_linter default_undesirable_operators T_and_F_symbol_linter object_length_linter
#' @seealso \code{\link{check}}, \code{\link{lint}}
#' @export
lintrRules <- function(allowUndesirable = FALSE) {
  # names = deprecated functions, values = replacement hint
  deprecatedFunctions <- list(fulldim = "use magclass::getItems()",
                              getRegionList = "use magclass::getItems()",
                              getRegions = "use magclass::getItems()",
                              `getRegions<-` = "use magclass::getItems()")

  # by default on.exit replaces all code registered to be run on exit,
  # making it very dangerous to use on.exit together with withr functions
  undesirableFunctions <- list(`on.exit` = "use withr::defer",
                               globalVariables = "rewrite code without global variables",
                               q = "use `return` or `stop` instead",
                               quit = "use `return` or `stop` instead")

  undesirableOperators <- list(`->` = NA)

  linters <- linters_with_defaults(absolute_path_linter(),
                                   line_length_linter(120),
                                   object_name_linter(styles = "camelCase"),
                                   todo_comment_linter(),
                                   cyclocomp_linter(25),
                                   T_and_F_symbol_linter(),
                                   object_length_linter(100))

  if (allowUndesirable) {
    linters$undesirable_function_linter <- undesirable_function_linter(deprecatedFunctions)
  } else {
    linters$undesirable_operator_linter <- undesirable_operator_linter(c(default_undesirable_operators,
                                                                         undesirableOperators))
    linters$undesirable_function_linter <- undesirable_function_linter(c(default_undesirable_functions,
                                                                         undesirableFunctions,
                                                                         deprecatedFunctions))
  }
  return(linters)
}
