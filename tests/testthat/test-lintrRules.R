test_that("lintrRules works", {
  expect_true("line_length_linter" %in% names(lintrRules()))
  expect_false("line_length_linter" %in% names(lintrRules(skip = c("line_length_linter"))))
})
