test_that("readme can be created from package", {
  expect_invisible(r <- package2readme("lucode2"))
  expect_gt(length(r), 30)
  expect_gt(sum(nchar(r)), 1500)

  r <- paste0(r, collapse = "\n")
  expect_match(r, "lucode2")
  expect_match(r, "Dietrich")
  expect_match(r, "Installation")
  expect_match(r, "version")
  expect_match(r, "CRAN status")
  expect_match(r, "BibTeX")
  expect_match(r, "install")
})

test_that("readme title does not contain newlines", {
  local_mocked_bindings(
    desc = function(...) {
      originalDesc <- desc::desc(...)
      originalDesc$set("Title", "a\n\tzZzZz \nc")
      return(originalDesc)
    }
  )
  expect_invisible(r <- package2readme("lucode2"))
  expect_match(paste0(r, collapse = "\n"), "a zZzZz c")
})
