isMadratPackage <- function(lib = ".") {
  isMadratPackage <- file.exists(file.path(lib, "R", "madrat.R"))
  isMadrat <- file.exists(file.path(lib, "R", "madrat-package.R"))
  return(isMadratPackage || isMadrat)
}

# lucode2 itself has no root-level CONTRIBUTING.md to sync via conditionalCopy(),
# so edit inst/extdata/CONTRIBUTING.md directly.
addContributingFiles <- function(lib = ".") {
  withr::local_dir(lib)

  conditionalCopy("CODE_STYLE.md")

  if (isMadratPackage()) {
    conditionalCopy("CONTRIBUTING.md")
    if (!"^CONTRIBUTING\\.md$" %in% readLines(".Rbuildignore")) {
      write("^CONTRIBUTING\\.md$", ".Rbuildignore", append = TRUE)
    }
  }

  return(invisible(NULL))
}
