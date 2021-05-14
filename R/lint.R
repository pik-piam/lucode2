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
lint <- function(filesToLint = getFilesToLint(), useLintrConfigFile = FALSE) {
  if (useLintrConfigFile) {
    # Using a config file instead of explicitly setting linter config here (in code) has the advantage of also being
    # respected when linting this project in another way, for example lintr::lint(path) or devtools::lint().
    # Also, a project could customize its .lintr to better fit the project's needs. However, this could lead to
    # different quality standards in different repos, which contradicts the purpose of lucode2::buildLibrary, which
    # tries to achieve a similar software quality in all repos.
    # TODO if we want to go this route, add documentation about the .lintr config file
    gitRoot <- system("git rev-parse --show-toplevel", intern = TRUE)
    linterConfigPath <- paste0(gitRoot, "/.lintr")
    if (!file.exists(linterConfigPath)) {
      cat(paste("linters: with_defaults(",
                "  line_length_linter = line_length_linter(120),",
                '  object_name_linter = object_name_linter(styles = "camelCase"),',
                "  cyclocomp_linter = NULL",
                "  )\n", sep = "\n"), file = linterConfigPath)

      rbuildignorePath <- paste0(gitRoot, "/.Rbuildignore")
      cat("^\\.lintr$\n", file = rbuildignorePath, append = TRUE)

      system(paste("git add", linterConfigPath, rbuildignorePath))
      cat("Created linter config file '.lintr' and added it to '.Rbuildignore'. Please commit these files.\n")
    }
    return(sapply(filesToLint, lintr::lint))
  } else {
    linters <- lintr::with_defaults(line_length_linter = lintr::line_length_linter(120),
                                    object_name_linter = lintr::object_name_linter(styles = "camelCase"),
                                    cyclocomp_linter = NULL)
    return(sapply(filesToLint, lintr::lint, linters = linters))
  }
}
