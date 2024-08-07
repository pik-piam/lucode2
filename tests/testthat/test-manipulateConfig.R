
test_that("manipulateConfig works", {
  withr::with_tempdir({
    configFile <- "test.gms"

    cfg <- list(foo      = 0,
                bar      = 0,
                buzz     = 0,
                bazz     = 0,
                bla_blub = 0,
                bla      = 0)

    code <- function(cfg) {
      x <- c("scalars",
             paste0("  foo  blah / ", cfg$foo, " /"),
             '  bar  "either x/y or X/Y"',
             '  bazz "just x/y"',
             paste0('  buzz "not x/y" / ', cfg$buzz, " /"),
             paste0("bla_blub example text (1) / ", cfg$bla_blub,  " /"),
             paste0("bla/ ", cfg$bla,  " /"),
             ";",
             paste0("bar  = ", cfg$bar, ";"),
             paste0("bazz = ", cfg$bazz, ";"),
             paste0("buzz = ", cfg$buzz, ";"),
             paste0("bla_blub = ", cfg$bla_blub, ";"),
             paste0("bla = ", cfg$bla, ";"),
             "*** bla = shouldstaylikethis",
             "*' not for bazz = shouldnotchange",
             paste0("bla=   ", cfg$bla, ";"))
      return(x)
    }

    writeLines(code(cfg), configFile)
    cfg[seq_along(cfg)] <- seq_along(cfg)
    manipulateConfig(configFile, cfg)
    expect_identical(readLines(configFile), code(cfg))
  })
})
