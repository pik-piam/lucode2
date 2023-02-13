test_that("fixBuildLibraryMergeConflict works", {
  tempFolder <- withr::local_tempdir()
  writeLines(c("<<<<<<< HEAD",
               "ValidationKey: '40481316'",
               "=======",
               "ValidationKey: '40597800'",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7",
               "AcceptedWarnings:",
               "- 'Warning: package ''.*'' was built under R version'"),
             file.path(tempFolder, ".buildlibrary"))
  writeLines(c("Package: madrat",
               "Type: Package",
               "Title: May All Data be Reproducible and Transparent (MADRaT) *",
               "<<<<<<< HEAD",
               "Version: 2.12.4",
               "Date: 2022-03-08",
               "=======",
               "Version: 2.13.0",
               "Date: 2022-03-09",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7"),
             file.path(tempFolder, "DESCRIPTION"))

  fixBuildLibraryMergeConflict(tempFolder)

  expect_identical(readLines(file.path(tempFolder, ".buildlibrary")),
                   c("ValidationKey: ''",
                     "AcceptedWarnings:",
                     "- 'Warning: package ''.*'' was built under R version'"))
  expect_identical(readLines(file.path(tempFolder, "DESCRIPTION")),
                   c("Package: madrat",
                     "Type: Package",
                     "Title: May All Data be Reproducible and Transparent (MADRaT) *",
                     "Version: 2.13.0",
                     "Date: 2022-03-09"))

  writeLines(c("<<<<<<< HEAD",
               "ValidationKey: '40481316'",
               "=======",
               "ValidationKey: '40597800'",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7",
               "AcceptedWarnings:",
               "- 'Warning: package ''.*'' was built under R version'"),
             file.path(tempFolder, ".buildlibrary"))
  writeLines(c("Package: madrat",
               "Type: Package",
               "Title: May All Data be Reproducible and Transparent (MADRaT) *",
               "<<<<<<< HEAD",
               "Version: 2.14.4",
               "Date: 2022-03-08",
               "=======",
               "Version: 2.13.0",
               "Date: 2022-03-09",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7"),
             file.path(tempFolder, "DESCRIPTION"))

  fixBuildLibraryMergeConflict(tempFolder)

  expect_identical(readLines(file.path(tempFolder, "DESCRIPTION")),
                   c("Package: madrat",
                     "Type: Package",
                     "Title: May All Data be Reproducible and Transparent (MADRaT) *",
                     "Version: 2.14.4",
                     "Date: 2022-03-08"))

  writeLines(c("<<<<<<< HEAD",
               "ValidationKey: '40481316'",
               "=======",
               "ValidationKey: '40597800'",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7",
               "AcceptedWarnings:",
               "- 'Warning: package ''.*'' was built under R version'"),
             file.path(tempFolder, ".buildlibrary"))
  writeLines(c("Package: madrat",
               "Type: Package",
               "Title: May All Data be Reproducible and Transparent (MADRaT) *",
               "<<<<<<< HEAD",
               "Version: 2.14.4",
               "pusteblume",
               "=======",
               "Version: 2.13.0",
               ">>>>>>> a1cb66cbf5f9ed75e57271542662266ac63841d7",
               "Date: 2022-03-09"),
             file.path(tempFolder, "DESCRIPTION"))

  fixBuildLibraryMergeConflict(tempFolder)

  expect_identical(readLines(file.path(tempFolder, "DESCRIPTION")),
                   c("Package: madrat",
                     "Type: Package",
                     "Title: May All Data be Reproducible and Transparent (MADRaT) *",
                     "Version: 2.14.4",
                     "pusteblume",
                     "Date: 2022-03-09"))
})
