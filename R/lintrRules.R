#' @importFrom lintr linters_with_defaults absolute_path_linter line_length_linter object_name_linter
#' todo_comment_linter undesirable_function_linter cyclocomp_linter default_undesirable_functions
#' undesirable_operator_linter default_undesirable_operators T_and_F_symbol_linter
#' @export
lintrRules <- function(allowUndesirable = FALSE) {
  # names = deprecated functions, values = replacement hint
  deprecatedFunctions <- list(`on.exit` = "use withr::defer",
                              # by default on.exit replaces all code registered to be run on exit,
                              # making it very dangerous to use on.exit together with withr functions
                              fulldim = "use magclass::getItems()",
                              getRegionList = "use magclass::getItems()",
                              getRegions = "use magclass::getItems()",
                              `getRegions<-` = "use magclass::getItems()")

  linters <- linters_with_defaults(absolute_path_linter(),
                                   line_length_linter(120),
                                   object_name_linter(styles = "camelCase"),
                                   todo_comment_linter(),
                                   cyclocomp_linter(25),
                                   T_and_F_symbol_linter())

  if (allowUndesirable) {
    linters$undesirable_function_linter <- undesirable_function_linter(deprecatedFunctions)
  } else {
    linters$undesirable_operator_linter <- undesirable_operator_linter(c(default_undesirable_operators,
                                                                         list(`->` = NA)))
    linters$undesirable_function_linter <- undesirable_function_linter(c(default_undesirable_functions,
                                                                         deprecatedFunctions))
  }
  return(linters)
}
