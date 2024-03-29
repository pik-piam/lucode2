#' Package version check tool
#'
#' Checks if there are CRAN-packages with the same name as those in our PIK-CRAN whose version is newer
#'
#' @param mail whether an email notification is sent to RSE
#' @param test to test whether auto email sending works
#' @param gitpath if an email notification has to be sent out, the path to the git repo
#' @author Anastasis Giannousakis
#' @export
#' @importFrom utils available.packages
#'
check_versions <- function(mail = TRUE, # nolint: object_name_linter.
                           test = FALSE,
                           gitpath = NULL) {
  a <- available.packages("https://rse.pik-potsdam.de/r/packages/src/contrib")

  cran <- suppressWarnings(available.packages("https://cloud.r-project.org/src/contrib", filters = "duplicates"))

  same <- intersect(rownames(a), rownames(cran))

  newer <- NULL

  for (i in same) {
    if (package_version(cran[i, "Version"]) > package_version(a[i, "Version"])) {
      newer[i] <- i
    }
  }

  file <- paste0(tempdir(), "/README.md")

  if (length(newer) != 0) {
    warning(c("Following packages have a newer version number on CRAN: ", paste0(newer, collapse = " ")))
    writeLines(c("WARNING, following packages have a newer version number on CRAN: ", paste0(newer, collapse = " ")),
               file)
    if (mail) {
      sendmail(path = gitpath, file = file, commitmessage = "PACKAGE WARNING", remote = TRUE, reset = TRUE)
    }
  } else {
    message("No CRAN packages with newer versions than our RSE packages found\n")
  }
  if (test) {
    writeLines("This is a test mail", file)
  }
  if (test) {
    sendmail(path = gitpath, file = file, commitmessage = "TEST: PACKAGE WARNING", remote = TRUE, reset = TRUE)
  }
}
