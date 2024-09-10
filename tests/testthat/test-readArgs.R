test_that("Check readArgs", {
  value1 <- "old"
  value2 <- 2
  value3 <- "willstaythesame"
  flags <- readArgs("value1", "value2", "value4", .flags = c(t = "--test", p = "--parallel"),
                    .argv = c("-t", "--blablub", "--parallel", "value1=new", "value2=3", "value3=isnotallowed"))
  expect_true("--test" %in% flags)
  expect_true("--parallel" %in% flags)
  expect_false("--blablub" %in% flags)
  expect_false("-t" %in% flags)
  expect_identical(value1, "new")
  expect_identical(value2, 3)
  expect_identical(value3, "willstaythesame")
  expect_false(exists("value4"))
  expect_message(readArgs(), "### READ COMMAND LINE - ASSIGNED CONFIGURATION ###")
  expect_message(readArgs(), "### READ COMMAND LINE - CONFIGURATION END ###")
  expect_message(readArgs("value1", .argv = c("value1=new")), "value1 <- new")
  expect_message(readArgs("value1", .argv = c("--value1=new")), "value1 <- new")
  expect_message(readArgs("value2", .argv = c("value2=3")), c("value2 <- 3"))
  expect_message(readArgs("value2", .argv = c("--value2=3")), c("value2 <- 3"))
  expect_message(readArgs("value4", .argv = NULL), c("value4 not defined"))
  expect_message(readArgs(.flags = c(t = "--test"), .argv = c("--test")), "Flags: --test")
  expect_message(readArgs(.flags = c(t = "--test", p = "--parallel"), .argv = c("-tp")), "Flags: --parallel, --test")
  expect_message(readArgs(.argv = c("--blablub")), "Unknown flags: --blablub")
})
