
test_that("manipulateConfig works", {
  withr::with_tempdir({
    configFile <- "test.gms"

    cfg <- list(foo  = 0,
                bar  = 0,
                buzz = 0,
                bazz = 0)

    code <- function(cfg) {
      x <- c("scalars",
             paste0("  foo  blah / ", cfg$foo, " /"),
             '  bar  "either x/y or X/Y"',
             '  bazz "just x/y"',
             paste0('  buzz "not x/y" / ', cfg$buzz, " /"),
             ";",
             paste0("bar  = ", cfg$bar, ";"),
             paste0("bazz = ", cfg$bazz, ";"),
             paste0("buzz = ", cfg$buzz, ";"))
      return(x)
    }

    writeLines(code(cfg), configFile)
    cfg[seq_along(cfg)] <- seq_along(cfg)
    manipulateConfig(configFile, cfg)
    expect_identical(readLines(configFile), code(cfg))
  })
})
