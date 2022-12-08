#' autoFormat
#'
#' Apply auto-formatting using styler::style_file to the given files. Does not change indentation.
#'
#' @param files A character vector of paths to files that should be auto-formatted.
#' @param ignoreLintFreeFiles If set to TRUE (the default) files without linter warnings are not auto-formatted.
#' @param lintAfterwards If set to TRUE (the default) return linter results for the auto-formatted files.
#' @param askTF Ask about whether to replace T/F with TRUE/FALSE, or just do it
#'     (default).
#'
#' @author Pascal Führlich
#' @seealso \code{\link{getFilesToLint}}
#' @importFrom styler style_file
#' @examples
#' \dontrun{
#' lucode2::autoFormat()
#' }
#' @export
autoFormat <- function(files = getFilesToLint(), ignoreLintFreeFiles = TRUE,
                       lintAfterwards = TRUE, askTF = FALSE) {

  # fix T_and_F_symbol lint (will only touch files with that kind of lint)
  theOneAndonlyTandFsymbolFixer(files, askTF)

  if (ignoreLintFreeFiles) {
    # keep only files with linter warnings
    files <- files[vapply(files, function(aFile) length(lint(aFile)) > 0, logical(1))]
  }

  # strict = FALSE -> keep alignment spaces around assignments, keep extra newlines
  # scope is set to not change indentation, otherwise the alignment of continuation lines would be messed up
  style_file(files, strict = FALSE, scope = I(c("tokens", "line_breaks", "spaces")))
  message("Hint: In RStudio you can fix indentation in selected lines using ctrl + i.")

  if (lintAfterwards) {
    print(lint(files))
  }
}

# Fix all instances of T/F rather than TRUE/FALSE indicated by
# lintr::T_and_F_symbol_linter().
theOneAndonlyTandFsymbolFixer <- function(files, ask = FALSE) {

  # get all files with this kind of lint
  theLint <- lapply(files, lintr::lint,
                    linters = list(lintr::T_and_F_symbol_linter()))

  # for each file
  for (f in theLint) {
    # read the file text
    t <- readLines(f[[1]][["filename"]])
    mod <- FALSE
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
        u <- paste0(
          substr(t[l], 0, r[1] - 1),
          switch(substr(t[l], r[1], r[2] - 1), "T" = "TRUE", "F" = "FALSE"),
          substr(t[l], r[2], nchar(t[l]))
        )

        # if told to ask, ask about replacing or not
        if (ask) {
          message(paste("Replace", t[l], "with", u,
                        paste0("in ", f[[1]][["filename"]], "? (Y/n)"),
                        sep = "\n"))
          # if not "yes, please", go to next occurrence
          if (!tolower(getLine()) %in% c("", "y", "yes")) {
            next
          }
        }

        # if not told to ask, or told to replace, replace
        t[l] <- u
        mod <- TRUE
      }

      # if the text was modified, write it back out
      if (mod) {
        writeLines(t, f[[1]][["filename"]])
      }
    }
  }
}
