#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files. Does not change indentation.
#'
#' @param files A character vector of paths to files that should be auto-formatted.
#' @param ignoreLintFreeFiles If set to TRUE (the default) files without linter warnings are not auto-formatted.
#' @param lintAfterwards If set to TRUE (the default) return linter results for the auto-formatted files.
#'
#' @author Pascal Sauer
#' @seealso \code{\link{getFilesToLint}}
#' @examples
#' \dontrun{
#' lucode2::autoFormat()
#' }
#' @export
autoFormat <- function(files = getFilesToLint(), ignoreLintFreeFiles = TRUE,
                       lintAfterwards = TRUE) {
  checkRequiredPackages("styler", "auto-formatting")

  # fix T_and_F_symbol lint (will only touch files with that kind of lint)
  theOneAndOnlyTandFsymbolFixer(files)

  if (ignoreLintFreeFiles) {
    # keep only files with linter warnings
    files <- unique(names(lint(files)))
  }

  # strict = FALSE -> keep alignment spaces around assignments, keep extra newlines
  # scope is set to not change indentation, otherwise the alignment of continuation lines would be messed up
  styler::style_file(files, strict = FALSE, scope = I(c("tokens", "line_breaks", "spaces")))
  message("Hint: In RStudio you can fix indentation in selected lines using ctrl + i.")

  if (lintAfterwards) {
    print(lint(files))
  }
}

# Fix all instances of T/F rather than TRUE/FALSE indicated by
# lintr::T_and_F_symbol_linter().
theOneAndOnlyTandFsymbolFixer <- function(files) {

  # get all files with this kind of lint
  theLint <- lapply(files, lintr::lint,
                    linters = list(lintr::T_and_F_symbol_linter()), parse_settings = FALSE)

  # for each file
  for (f in theLint) {
    if (0 == length(unlist(f))) {
      next
    }

    # read the file text
    t <- readLines(f[[1]][["filename"]])

    # for each occurrence within the file
    # lint is listed top-to-bottom, left-to-right; so proceed backwards through
    # the list because modifying lines messes up character columns otherwise
    for (i in rev(f)) {
      # get the line number
      l <- i[["line_number"]]

      # for each range within the line (not sure when there could be more than
      # one)
      for (r in rev(i[["ranges"]])) {
        # replace the lint
        t[l] <- paste0(
          substr(t[l], 0, r[1] - 1),
          switch(substr(t[l], r[1], r[2] - 1), "T" = "TRUE", "F" = "FALSE"),
          substr(t[l], r[2], nchar(t[l]))
        )
      }

      writeLines(t, f[[1]][["filename"]])
    }
  }
}
