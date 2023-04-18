test_that("findIterations works", {
  tmpfolder <- withr::local_tempdir()
  goodfolders <- c("SCEN-rem-1", "SCEN-rem-2", "SCEN-rem-20")
  badfolders <- c("SCEN-rem-1-failed", "SCEN-abc-rem-1-asdf", "SCEN-rem-asdf-1", "NOSCEN-rem-1", "TEST-rem-2")
  file.create(file.path(tmpfolder, "SCEN-rem-10.pdf"))
  for (f in c(goodfolders, badfolders)) {
    dir.create(file.path(tmpfolder, f))
    file.create(file.path(tmpfolder, f, "fulldata.gdx"))
  }
  found <- findIterations("SCEN", tmpfolder)
  expect_true(setequal(found, file.path(tmpfolder, goodfolders)))

  tmpfolder <- withr::local_tempdir()
  goodfolders <- c("SCEN-mag-1", "SCEN-mag-2", "SCEN-mag-20")
  badfolders <- c("SCEN-mag-1-failed", "SCEN-abc-mag-1-asdf", "SCEN-mag-asdf-1", "NOSCEN-mag-1", "TEST-mag-2")
  file.create(file.path(tmpfolder, "SCEN-mag-10.pdf"))
  for (f in c(goodfolders, badfolders)) {
    dir.create(file.path(tmpfolder, f))
    file.create(file.path(tmpfolder, f, "fulldata.gdx"))
  }
  found <- findIterations("SCEN", tmpfolder)
  expect_true(setequal(found, file.path(tmpfolder, goodfolders)))

  tmpfolder <- withr::local_tempdir()
  goodfolders <- c("SCEN1-mag-1", "SCEN2-rem-1")
  for (f in c(goodfolders, badfolders)) {
    dir.create(file.path(tmpfolder, f))
    file.create(file.path(tmpfolder, f, "fulldata.gdx"))
  }
  found <- findIterations(c("SCEN1", "SCEN2"), tmpfolder)
  expect_true(setequal(found, file.path(tmpfolder, goodfolders)))
})
