# A madrat-based package has read/calc/correct/full functions run via madrat
# (readSource/calcOutput/retrieveData). CONTRIBUTING.md is only relevant to those.
isMadratPackage <- function(lib = ".") {
  file.exists(file.path(lib, "R", "madrat.R"))
}

# CODE_STYLE.md goes into every package (its .Rbuildignore entry lives centrally in
# loadBuildLibraryConfig.R). CONTRIBUTING.md only goes into madrat-based packages, since it
# documents madrat-specific workflows. lucode2 itself isn't madrat-based and has no root-level
# CONTRIBUTING.md to sync via conditionalCopy(), so edit inst/extdata/CONTRIBUTING.md directly.
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
