
test_that("getScenNames works", {
  withr::with_tempdir({
    # set up testing environment
    dir.create("output1")
    dir.create("output2")
    dir.create("output3")
    cfg <- list("Name 1", "value")
    names(cfg) <- c("title", "otherKey")
    save(cfg, file = "output1/config.Rdata")

    stuff <- list("value")
    names(stuff) <- c("otherKey")
    save(stuff, file = "output2/config.Rdata")

    cfg <- list("Name 3")
    names(cfg) <- c("title")
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
