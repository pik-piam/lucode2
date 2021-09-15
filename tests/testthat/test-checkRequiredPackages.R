test_that("checkRequiredPackages works", {
  requireNamespace("waldo", quietly = TRUE) # load waldo package needed for expect_false before setting .libPaths
  originalLibPaths <- .libPaths()
  on.exit(.libPaths(originalLibPaths))
  .libPaths(tempdir(), FALSE)
  withr::local_options(c(repos = "https://cran.rstudio.com/"))
  # setting readlineFunction so that the answer is returned without asking
  expect_error(checkRequiredPackages("mvcx94kdsfs83saids2al", readlineFunction = function(x) "n"),
               "The following package is required, but it is still not available:\n- mvcx94kdsfs83saids2al")
  expect_error(suppressWarnings(checkRequiredPackages("mvcx94kdsfs83saids2al", readlineFunction = function(x) "y")),
               "The following package is required, but it is still not available:\n- mvcx94kdsfs83saids2al")

  expect_error(checkRequiredPackages("gtools", readlineFunction = function(x) "n"),
               "The following package is required, but it is still not available:\n- gtools")
  expect_false(requireNamespace("gtools", quietly = TRUE))
  expect_silent(checkRequiredPackages("gtools",
                                      readlineFunction = function(x) "y",
                                      installFunction = function(...) install.packages(..., quiet = TRUE)))
  expect_true(requireNamespace("gtools", quietly = TRUE))
})
