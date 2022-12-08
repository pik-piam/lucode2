test_that(
  desc = "Test theOneAndOnlyTandFsymbolFixer()",

  code = {
    tmpFile1 <- tempfile(pattern = "fool", fileext = ".R")
    tmpFile2 <- tempfile(pattern = "fool", fileext = ".R")
    fool <- c(
      "if (T || F) {",
      "  print(\"Mister T pitties the fool replacing every T in a file!\")",
      "}")

    wool <- c(
      "if (TRUE || FALSE) {",
      "  print(\"Mister T pitties the fool replacing every T in a file!\")",
      "}")

    writeLines(text = fool, con = tmpFile1)
    theOneAndOnlyTandFsymbolFixer(tmpFile1)

    expect_identical(object = readLines(tmpFile1), expected = wool)

    # test again that lint-free files pass
    writeLines(text = fool, con = tmpFile2)
    expect_no_error(theOneAndOnlyTandFsymbolFixer(c(tmpFile1, tmpFile2)))
  }
)
