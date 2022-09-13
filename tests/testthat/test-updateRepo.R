test_that("updateRepo works", {
  capture.output({
    reposFolder <- withr::local_tempdir()

    gert::git_clone("https://github.com/pik-piam/miniPackage.git",
                    file.path(reposFolder, "miniPackage"),
                    verbose = FALSE)
    expect_message({
      expect_error(updateRepo(reposFolder),
                   "There were errors, see log (on RSE server with `journalctl -r -u update-repo.service`)",
                   fixed = TRUE)
    }, "invalid validation key")

    writeLines("ValidationKey: '173196000'", file.path(reposFolder, "miniPackage", ".buildlibrary"))
    updateRepo(reposFolder)
    expect_true(file.exists(file.path(reposFolder, "miniPackage_0.0.0.9000.tar.gz")))
    expect_true(file.exists(file.path(reposFolder, "PACKAGES")))

    expect_message(updateRepo(reposFolder), "miniPackage 0.0.0.9000 ok")
    usethis::with_project(file.path(reposFolder, "miniPackage"), {
      usethis::use_version("major")
    })
    writeLines("ValidationKey: '1924400'", file.path(reposFolder, "miniPackage", ".buildlibrary"))
    expect_message(updateRepo(reposFolder), "miniPackage 0.0.0.9000 -> 1.0.0 build success", fixed = TRUE)
  })
})
