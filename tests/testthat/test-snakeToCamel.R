test_that("snakeToCamel works", {
  withr::with_tempdir({
    writeLines(c("a_s_D <- 5045",
                 "aSD",
                 "a_s_D + a_s_D^3"), "test.R")
    expect_warning({
      actual <- snakeToCamel("test.R", ask = FALSE)
    }, "aSD is already present in the original file:")
    expect_identical(actual,
                     c("aSD <- 5045",
                       "aSD",
                       "aSD + aSD^3"))
    expect_identical(readLines("test.R"),
                     c("aSD <- 5045",
                       "aSD",
                       "aSD + aSD^3"))
  })
})
