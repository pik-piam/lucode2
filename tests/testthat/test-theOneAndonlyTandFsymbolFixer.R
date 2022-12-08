test_that(
  desc = "Test theOneAndonlyTandFsymbolFixer()",

  code = {
    tmpFile <- tempfile(pattern = "fool", fileext = ".R")
    fool <- c(
      "if (T || F) {",
      "  print(\"Mister T pitties the fool replacing every T in a file!\")",
      "}")

    wool <- c(
      "if (TRUE || FALSE) {",
      "  print(\"Mister T pitties the fool replacing every T in a file!\")",
      "}")

    writeLines(text = fool, con = tmpFile)
    theOneAndonlyTandFsymbolFixer(tmpFile)

    expect_identical(object = readLines(tmpFile),
                     expected = wool)
  }
)
