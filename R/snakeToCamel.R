#' snakeToCamel
#'
#' Convenience function to rename variables in an R file from snake to camel case.
#'
#' @param pathToFile Path to the R source file where variables should be renamed.
#' @param ask If TRUE (default) ask before renaming a variable, otherwise always assume "yes" as answer.
#'
#' @importFrom dplyr %>%
#' @importFrom utils askYesNo
#' @export
snakeToCamel <- function(pathToFile, ask = TRUE) {
  lines <- readLines(pathToFile)
  variablePattern <- "(^|^.*?[^\"])\\b([a-zA-Z._0-9]*)\\b *<-.*$"
  variables <- grep(variablePattern, lines, value = TRUE) %>%
    sub(pattern = variablePattern, replacement = "\\2", perl = TRUE) %>%
    Filter(f = function(variable) nchar(variable) > 0) %>%
    unique()
  replacements <- variables %>%
    gsub(pattern = "_([a-z])", replacement = "\\U\\1", perl = TRUE) %>%
    sub(pattern = "^([A-Z])", replacement = "\\L\\1", perl = TRUE) %>%
    gsub(pattern = "_", replacement = "")

  keepIndex <- vapply(seq_along(variables), function(i) variables[[i]] != replacements[[i]], logical(1))
  variables <- variables[keepIndex]
  replacements <- replacements[keepIndex]

  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    problematicReplacements <- grep(pattern = paste0("\\b", variable, "\\b"), x = replacements, value = TRUE)
    if (length(problematicReplacements) > 0) {
      warning(variable, ":\n", paste(problematicReplacements, collapse = "\n"), "\nend of warning.", immediate. = TRUE)
    }
    hitsBeforeReplacement <- grep(pattern = paste0("\\b", replacements[[i]], "\\b"),
                                  lines, value = TRUE)
    if (length(hitsBeforeReplacement) > 0) {
      warning(replacements[[i]], " is already present in the original file:\n",
              paste("  ", hitsBeforeReplacement, collapse = "\n"), immediate. = TRUE)
    }
    message(variables[[i]], " -> ", replacements[[i]])
    if (!ask || askYesNo("replace?")) {
      lines <- gsub(paste0('(?<!["', "'])\\b", variables[[i]], "\\b"), replacements[[i]], lines, perl = TRUE)
      writeLines(lines, pathToFile)
    }
  }
  return(invisible(lines))
}
