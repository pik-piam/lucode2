test_that("checkRequiredPackages works", {
  # setting readlineFunction so that the answer is returned without asking
  expect_error(checkRequiredPackages("nonexistentpackage", readlineFunction = function(question) "n"),
               "The following package is required, but it is still not available:\n- nonexistentpackage")
  expect_error(checkRequiredPackages("nonexistentpackage",
                                     installFunction = function(...) 0, # do nothing to mimic failed installation
                                     readlineFunction = function(question) "y"),
               "The following package is required, but it is still not available:\n- nonexistentpackage")

  temporaryLibPath <- withr::local_tempdir()
  unloadNamespace("gtools")
  unloadNamespace("ggplot2")

  expect_error(checkRequiredPackages("gtools", readlineFunction = function(question) "n", libPaths = temporaryLibPath),
               "The following package is required, but it is still not available:\n- gtools")

  expect_error(
    checkRequiredPackages(c("gtools", "ggplot2"), requiredFor = "something nice",
                          readlineFunction = function(question) {
                            expect_identical(question, paste(
                              "The following currently not installed packages are required for something nice:",
                              "- gtools",
                              "- ggplot2",
                              "Do you want to install them now? (y/N)", sep = "\n"))
                            return("") # answer = "" should trigger the default (FALSE)
                          },
                          libPaths = temporaryLibPath),
    paste("The following packages are required for something nice, but they are still not available:",
          "- gtools",
          "- ggplot2", sep = "\n"))

  expect_false(requireNamespace("gtools", lib.loc = temporaryLibPath, quietly = TRUE))

  withr::local_options(c(repos = "https://cran.rstudio.com/"))
  on.exit({
    unloadNamespace("gtools") # prevent warning; temporaryLibPath (where gtools is loaded from) will be deleted
  })
  checkRequiredPackages("gtools", requiredFor = "something cool",
                        readlineFunction = function(question) {
                          expect_identical(question, paste(
                            "The following currently not installed package is required for something cool:",
                            "- gtools",
                            "Do you want to install it now? (y/N)", sep = "\n"))
                          return("y")
                        },
                        installFunction = function(...) install.packages(..., quiet = TRUE),
                        libPaths = temporaryLibPath)
  expect_true(requireNamespace("gtools", lib.loc = temporaryLibPath, quietly = TRUE))

  expect_error(
    checkRequiredPackages(c("gtools", "ggplot2"), requiredFor = "something important",
                          readlineFunction = function(question) {
                            expect_identical(question, paste(
                              "The following currently not installed package is required for something important:",
                              "- ggplot2", # gtools was already installed, so only ggplot2 missing
                              "Do you want to install it now? (y/N)", sep = "\n"))
                            return("anything") # any answer except y/yes -> FALSE
                          },
                          libPaths = temporaryLibPath),
    paste("The following package is required for something important, but it is still not available:",
          "- ggplot2", sep = "\n")) # gtools was already installed, so only ggplot2 missing
})
