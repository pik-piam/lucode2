# lint

Check the given files for linter warnings using lintr::lint.

## Usage

``` r
lint(files = getFilesToLint())
```

## Arguments

- files:

  A character vector of paths to files that should be checked by the
  linter. If set to "." the whole package is linted.

## Value

A named list, where the names are the paths to the linted files and the
values are lists containing the linter warnings.

## Details

For files in the vignettes and tests folder less strict rules are
applied, e.g. using ::: usually leads to a linter warning, but not in
vignettes/tests. Which linter rules are used depends on ".lintr" config
files. [`check`](check.md) creates lintr config files that use
[`lintrRules`](lintrRules.md).

## See also

[`getFilesToLint`](getFilesToLint.md), [`lintrRules`](lintrRules.md),
[`autoFormat`](autoFormat.md),
[`lint`](https://lintr.r-lib.org/reference/lint.html)

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
lucode2::lint()
} # }
```
