modifyRproj <- function(lib = ".") {
  pathToRproj <- Sys.glob(file.path(lib, "*.Rproj"))
  if (length(pathToRproj) != 1) {
    return(invisible(NULL))
  }
  lines <- readLines(pathToRproj)
  if (!any(grepl("^AutoAppendNewline: ", lines))) {
    lines <- c(lines, "AutoAppendNewline: Yes")
  }
  if (!any(grepl("^StripTrailingWhitespace: ", lines))) {
    lines <- c(lines, "StripTrailingWhitespace: Yes")
  }
  writeLines(lines, pathToRproj)
}
