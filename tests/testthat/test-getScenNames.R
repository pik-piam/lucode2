
test_that("getScenNames works", {
  withr::with_tempdir({
    # set up testing environment
    dir.create("output1")
    dir.create("output2")
    dir.create("output3")

    cfg <- list("title" = "Name 1", "otherKey" = "value")
    save(cfg, file = "output1/config.Rdata")

    stuff <- list("otherKey" = "value")
    save(stuff, file = "output2/config.Rdata")

    cfg <- list("title" = "Name 3")
    save(cfg, file = "output3/config.Rdata")

    expected <- c("Name 1", "Name 3")
    names(expected) <- c("output1/config.Rdata",
                         "output3/config.Rdata")
    expect_identical(
      expected,
      getScenNames(c("output1", "output3"))
    )

    expect_error(getScenNames(c("output1", "output2", "output3")))
  })
})
