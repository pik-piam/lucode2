#' gitAuthors
#'
#' Print each git author's first name, last name, and mail address in a way that could be copy pasted
#' into DESCRIPTION. Please note that this list usually still needs to be carefully checked
#' e.g. for duplicates and cleaned.
#'
#' @return Invisibly, a data.frame with the following columns: raw, firstName, lastName, mailAdress.
#'
#' @author Pascal Führlich
#' @export
gitAuthors <- function() {
  if (Sys.which("git") == "") {
    stop("git is not available")
  } else if (system2("git", "status", stdout = NULL, stderr = NULL) != 0) {
    stop(normalizePath("."), " is not part of a git repository")
  }
  rawGitAuthors <- system2("git", c("shortlog", "-s", "-n", "-e"), stdout = TRUE)
  pattern <- "^.+\t(?<firstName>.*?) ?(?<lastName>[^ ]+) \\<(?<mailAdress>[^<>]+)\\>$"
  matches <- regmatches(rawGitAuthors, gregexec(pattern, rawGitAuthors, perl = TRUE))
  firstName <- vapply(matches, function(x) x["firstName", ], character(1))
  lastName <- vapply(matches, function(x) x["lastName", ], character(1))
  mailAdress <- vapply(matches, function(x) x["mailAdress", ], character(1))
  message("Authors@R: c(\n",
          paste0('    person("', firstName, '", "', lastName, '", , "', mailAdress, '", role = "aut")',
                 collapse = ",\n"),
          "\n  )")
  return(invisible(data.frame(raw = rawGitAuthors,
                              firstName = firstName,
                              lastName = lastName,
                              mailAdress = mailAdress)))
}
