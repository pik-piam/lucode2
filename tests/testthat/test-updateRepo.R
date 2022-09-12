test_that("updateRepo works", {
  capture.output({
    reposFolder <- withr::local_tempdir()

    gert::git_clone("https://github.com/pik-piam/miniPackage.git",
                    file.path(reposFolder, "miniPackage"),
                    verbose = FALSE)
    expect_output({
      expect_error(updateRepo(reposFolder),
                  "There were errors, see log (on RSE server with `journalctl -u update-repo.service`)",
                  fixed = TRUE)
    }, "invalid validation key")

    writeLines("ValidationKey: '173196000'", file.path(reposFolder, "miniPackage", ".buildlibrary"))
    updateRepo(reposFolder)
    expect_true(file.exists(file.path(reposFolder, "miniPackage_0.0.0.9000.tar.gz")))
    expect_true(file.exists(file.path(reposFolder, "PACKAGES")))
  })
})
