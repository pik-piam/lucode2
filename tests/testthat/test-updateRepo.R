test_that("updateRepo works", {
  capture.output({
    baseFolder <- withr::local_tempdir()
    reposFolder <- file.path(baseFolder, "repos")
    miniPackageRepo <- file.path(baseFolder, "repo-miniPackage")

    # The following indirection via a local repo makes this test less
    # susceptible to system-local libgit issues.
    system(paste0("git clone --depth=1 https://github.com/pik-piam/miniPackage.git", " ", miniPackageRepo),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    gert::git_clone(miniPackageRepo,
                    file.path(reposFolder, "miniPackage"),
                    verbose = FALSE)
    expect_message({
      expect_error(updateRepo(reposFolder),
                   "There were errors, see log (on RSE server with `journalctl -r -u update-repo.service`)",
                   fixed = TRUE)
    }, "invalid validation key")

    writeLines("ValidationKey: '173196000'", file.path(reposFolder, "miniPackage", ".buildlibrary"))
    updateRepo(reposFolder, clean = FALSE)
    expect_true(file.exists(file.path(reposFolder, "miniPackage_0.0.0.9000.tar.gz")))
    expect_true(file.exists(file.path(reposFolder, "PACKAGES")))

    expect_message(updateRepo(reposFolder, clean = FALSE), "miniPackage 0.0.0.9000 ok", fixed = TRUE)
    usethis::with_project(file.path(reposFolder, "miniPackage"), {
      usethis::use_version("major")
    })
    writeLines("ValidationKey: '1924400'", file.path(reposFolder, "miniPackage", ".buildlibrary"))
    expect_message(updateRepo(reposFolder, clean = FALSE),
                   "miniPackage 0.0.0.9000 -> 1.0.0 build success", fixed = TRUE)
    expect_message({
      # clean = TRUE will delete untracked .buildlibrary file with validationkey
      expect_error(updateRepo(reposFolder, clean = TRUE),
                   "There were errors, see log (on RSE server with `journalctl -r -u update-repo.service`)",
                   fixed = TRUE)
    }, "invalid validation key")
  })
})
