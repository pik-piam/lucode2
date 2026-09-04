test_that("addContributingFiles always adds CODE_STYLE.md, CONTRIBUTING.md only for madrat-based packages", {
  withr::local_dir(withr::local_tempdir())
  writeLines("Package: mrfake", "DESCRIPTION")
  dir.create("inst/extdata", recursive = TRUE)
  dir.create("R")
  writeLines("", ".Rbuildignore")

  # not madrat-based: CODE_STYLE.md is added, CONTRIBUTING.md is not
  addContributingFiles()
  expect_gt(length(readLines("CODE_STYLE.md")), 0)
  expect_false(file.exists("CONTRIBUTING.md"))
  expect_false("^CONTRIBUTING\\.md$" %in% readLines(".Rbuildignore"))

  # madrat-based: CONTRIBUTING.md also gets copied and .Rbuildignore updated
  writeLines("", "R/madrat.R")
  addContributingFiles()
  expect_gt(length(readLines("CONTRIBUTING.md")), 0)
  rbuildIgnore <- readLines(".Rbuildignore")
  expect_true("^CONTRIBUTING\\.md$" %in% rbuildIgnore)

  # idempotent: running again does not duplicate .Rbuildignore entries
  addContributingFiles()
  rbuildIgnore <- readLines(".Rbuildignore")
  expect_identical(sum(rbuildIgnore == "^CONTRIBUTING\\.md$"), 1L)

  # overwrite: hand-edited files are restored from extdata
  writeLines("local edit", "CODE_STYLE.md")
  addContributingFiles()
  expect_false("local edit" %in% readLines("CODE_STYLE.md"))
})

test_that("isMadratPackage detects R/madrat.R", {
  withr::local_dir(withr::local_tempdir())
  expect_false(isMadratPackage())
  dir.create("R")
  writeLines("", "R/madrat.R")
  expect_true(isMadratPackage())
})
