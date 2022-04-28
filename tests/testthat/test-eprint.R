test_that("Eprint works", {
  a <- 2
  expect_message(eprint("a"), "a <- 2")
  expect_message(eprint("b"), "b not found")
})
