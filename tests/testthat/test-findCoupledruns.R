test_that("findCoupledruns works", {
  tmpfolder <- withr::local_tempdir()
  goodfolders <- c("TEST-rem-1", "TEST-rem-2", "TEST5-rem-20")
  badfolders <- c("asd.abc-rem-1", "TEST-rem-1-asdf", "TEST-rem-asdf-1")
  file.create(file.path(tmpfolder, "TEST-rem-10.pdf"))
  for (f in c(goodfolders, badfolders)) {
    dir.create(file.path(tmpfolder, f))
  }
  found <- findCoupledruns(tmpfolder)
  expect_true(setequal(found, c("TEST", "TEST5")))

  tmpfolder <- withr::local_tempdir()
  goodfolders <- c("TEST-mag-1", "TEST-mag-2", "TEST5-mag-20")
  badfolders <- c("TEST-mag-1-asdf", "TEST-mag-asdf-1")
  file.create(file.path(tmpfolder, "TEST-mag-10.pdf"))
  for (f in c(goodfolders, badfolders)) {
    dir.create(file.path(tmpfolder, f))
  }
  found <- findCoupledruns(tmpfolder)
  expect_true(setequal(found, c("TEST", "TEST5")))
  file.create(file.path(tmpfolder, "TEST-rem-10.pdf"))
  expect_true(setequal(found, c("TEST", "TEST5")))
})
