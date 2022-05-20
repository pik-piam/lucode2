test_that("manipulateFile works", {
  filename <- "./filetomanipulate.txt"
  testtext <- "This is a testi test.\nI'm done."
  m <- list(list("test.", "result"))
  withr::with_tempdir({
    # as default
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m))
    expect_identical(readLines(filename), c("This is a result result", "I'm done."))
    # with perl = TRUE specified
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m, perl = TRUE))
    expect_identical(readLines(filename), c("This is a result result", "I'm done."))
    # with perl = TRUE and fixed = FALSE specified
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m, fixed = FALSE, perl = TRUE))
    expect_identical(readLines(filename), c("This is a result result", "I'm done."))
    # with fixed = TRUE, overwriting perl = TRUE
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m, fixed = TRUE, perl = TRUE))
    expect_identical(readLines(filename), c("This is a testi result", "I'm done."))
    # with fixed = FALSE and perl = FALSE
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m, fixed = FALSE, perl = FALSE))
    expect_identical(readLines(filename), c("This is a result result", "I'm done."))
    # with fixed = TRUE, overwriting the default perl = TRUE
    writeLines(testtext, filename)
    expect_silent(manipulateFile(filename, m, fixed = TRUE))
    expect_identical(readLines(filename), c("This is a testi result", "I'm done."))
  })
})
