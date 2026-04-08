# check

Builds documentation and runs checks, tests, and linter. Find solutions
to common problems at
https://github.com/pik-piam/discussions/discussions/18

## Usage

``` r
check(
  lib = ".",
  cran = TRUE,
  config = loadBuildLibraryConfig(lib),
  runLinter = TRUE
)
```

## Arguments

- lib:

  Path to the package

- cran:

  If cran-like test is needed

- config:

  A configuration defining AcceptedWarnings, AcceptedNotes, and
  allowLinterWarnings. By default the .buildLibrary file is read.

- runLinter:

  Set to FALSE to skip the linter.

## Details

This function builds documentation including vignettes via
devtools::document(). It runs devtools::check() (without tests), then in
a separate clean R session it runs devtools::test(), and finally
lucode2::lint(). Before linting ".lintr" config files are created if
missing. The actual linter rules are defined in
[`lintrRules`](lintrRules.md). In general undesirable functions and
operators result in linter warnings, but not in the tests and vignettes
subdirectories. Warnings and notes in checks and tests are only allowed
if the given config defines them as accepted, otherwise this function
will stop.

## See also

[`buildLibrary`](buildLibrary.md), [`lint`](lint.md),
[`lintrRules`](lintrRules.md)

[`check`](https://devtools.r-lib.org/reference/check.html),
[`test`](https://devtools.r-lib.org/reference/test.html)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
lucode2::check()
} # }
```
