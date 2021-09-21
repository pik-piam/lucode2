test_that("checkRequiredPackages works", {
  if (isNamespaceLoaded("gtools")) {
    pkgload::unload("gtools", quiet = TRUE)
  }
  if (isNamespaceLoaded("ggplot2")) {
    pkgload::unload("ggplot2")
  }
  requireNamespace("waldo", quietly = TRUE) # load waldo package needed for expect_false before setting .libPaths
  originalLibPaths <- .libPaths()
  on.exit(.libPaths(originalLibPaths))
  .libPaths(withr::local_tempdir(), FALSE)
  # setting readlineFunction so that the answer is returned without asking
  expect_error(checkRequiredPackages("nonexistantpackage", readlineFunction = function(x) "n"),
               "The following package is required, but it is still not available:\n- nonexistantpackage")
  expect_error(checkRequiredPackages("nonexistantpackage",
                                     installFunction = function(x) 0,
                                     readlineFunction = function(x) "y"),
               "The following package is required, but it is still not available:\n- nonexistantpackage")

  expect_error(checkRequiredPackages("gtools", readlineFunction = function(x) "n"),
               "The following package is required, but it is still not available:\n- gtools")

  expect_error(
    checkRequiredPackages(c("gtools", "ggplot2"), requiredFor = "something nice",
                          readlineFunction = function(question) {
                            expect_identical(question, paste(
                              "The following currently not installed packages are required for something nice:",
                              "- gtools",
                              "- ggplot2",
                              "Do you want to install them now? (y/N)", sep = "\n"))
                            return("")
                          }),
    paste("The following packages are required for something nice, but they are still not available:",
          "- gtools",
          "- ggplot2", sep = "\n"))

  expect_false(requireNamespace("gtools", quietly = TRUE))

  withr::local_options(c(repos = "https://cran.rstudio.com/"))
  checkRequiredPackages("gtools", requiredFor = "something cool",
                        readlineFunction = function(question) {
                          expect_identical(question, paste(
                            "The following currently not installed package is required for something cool:",
                            "- gtools",
                            "Do you want to install it now? (y/N)", sep = "\n"))
                          return("y")
                        },
                        installFunction = function(...) install.packages(..., quiet = TRUE))
  expect_true(requireNamespace("gtools", quietly = TRUE))

  expect_error(
    checkRequiredPackages(c("gtools", "ggplot2"), requiredFor = "something important",
                          readlineFunction = function(question) {
                            expect_identical(question, paste(
                              "The following currently not installed package is required for something important:",
                              "- ggplot2",
                              "Do you want to install it now? (y/N)", sep = "\n"))
                            return("anything")
                          }),
    paste("The following package is required for something important, but it is still not available:",
          "- ggplot2", sep = "\n"))
})
