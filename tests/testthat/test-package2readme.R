test_that("readme can be created from package", {
  expect_invisible(r <- suppressMessages(package2readme("lucode2")))
  expect_gt(length(r), 30)
  expect_gt(sum(nchar(r)), 1500)

  r <- paste0(r, collapse = "\n")
  expect_match(r, "lucode2")
  expect_match(r, "Dietrich")
  expect_match(r, "Installation")
  expect_match(r, "version")
  expect_no_match(r, "CRAN status")
  expect_match(r, "BibTeX")
  expect_match(r, "install")
})

test_that("CRAN badge shown when CRAN-SUBMISSION exists", {
  tmpdir <- withr::local_tempdir()
  tmpdir <- file.path(tmpdir, "fakepkg")
  dir.create(tmpdir)

  file.copy(system.file("DESCRIPTION", package = "lucode2"), tmpdir)
  d <- desc::desc(file.path(tmpdir, "DESCRIPTION"))
  d$set(Package = "fakepkg")
  d$write()
  file.create(file.path(tmpdir, "CRAN-SUBMISSION"))

  r <- paste0(suppressMessages(package2readme(tmpdir)), collapse = "\n")
  expect_match(r, "CRAN status")
})

test_that("hand written sections at the end of README.md are kept", {
  tmpdir <- withr::local_tempdir()
  tmpdir <- file.path(tmpdir, "fakepkg")
  dir.create(tmpdir)
  file.copy(system.file("DESCRIPTION", package = "lucode2"), tmpdir)
  d <- desc::desc(file.path(tmpdir, "DESCRIPTION"))
  d$set(Package = "fakepkg")
  d$write()
  suppressMessages(package2readme(tmpdir))

  readmeFile <- file.path(tmpdir, "README.md")
  readme <- readLines(readmeFile)
  writeLines(c(readme, "", "## Funding", "", "Funding text."), readmeFile)

  expect_message(suppressWarnings(package2readme(tmpdir)), "Kept custom README section: Funding")
  readme <- readLines(readmeFile)
  fundingStart <- grep("^## Funding$", readme)
  expect_length(fundingStart, 1)
  expect_equal(readme[fundingStart - 1], "") # single blank line as separator
  expect_true("Funding text." %in% readme)
  # generated sections are not duplicated
  expect_length(grep("^## Citation", readme), 1)

  # a second run keeps it, still exactly once
  suppressMessages(suppressWarnings(package2readme(tmpdir)))
  expect_length(grep("^## Funding$", readLines(readmeFile)), 1)

  # if it is deleted by hand, it stays deleted
  writeLines(readme[seq_len(fundingStart - 2)], readmeFile)
  suppressMessages(suppressWarnings(package2readme(tmpdir)))
  expect_length(grep("^## Funding$", readLines(readmeFile)), 0)
})

test_that("readme title does not contain newlines", {
  local_mocked_bindings(
    desc = function(...) {
      originalDesc <- desc::desc(...)
      originalDesc$set("Title", "a\n\tzZzZz \nc")
      return(originalDesc)
    }
  )
  expect_invisible(r <- suppressMessages(package2readme("lucode2")))
  expect_match(paste0(r, collapse = "\n"), "a zZzZz c")
})
