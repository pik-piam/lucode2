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
  variables <- lines[grepl("[a-zA-Z._0-9]*_[a-zA-Z._0-9]* <-", lines)] %>%
    sub(pattern = "^.*?([a-zA-Z._0-9]*) <-.*$", replacement = "\\1") %>%
    unique()
  replacements <- variables %>%
    gsub(pattern = "_([a-z])", replacement = "\\U\\1", perl = TRUE) %>%
    gsub(pattern = "_", replacement = "")

  for (i in seq_along(variables)) {
    hitsBeforeReplacement <- grep(replacements[[i]], lines, value = TRUE)
    if (length(hitsBeforeReplacement) > 0) {
      warning(replacements[[i]], " is already present in the original file.")
    }
    message(variables[[i]], " -> ", replacements[[i]])
    if (!ask || askYesNo("replace?")) {
      lines <- gsub(variables[[i]], replacements[[i]], lines, fixed = TRUE)
    }
  }
  if (!ask || askYesNo(paste0("Overwrite ", pathToFile, "?"))) {
    writeLines(lines, pathToFile)
    return(invisible(lines))
  }
  return(lines)
}
