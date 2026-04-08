# autoFormat

Apply auto-formatting using styler::style_file to the given files. Does
not change indentation.

## Usage

``` r
autoFormat(
  files = getFilesToLint(),
  ignoreLintFreeFiles = TRUE,
  lintAfterwards = TRUE
)
```

## Arguments

- files:

  A character vector of paths to files that should be auto-formatted.

- ignoreLintFreeFiles:

  If set to TRUE (the default) files without linter warnings are not
  auto-formatted.

- lintAfterwards:

  If set to TRUE (the default) return linter results for the
  auto-formatted files.

## See also

[`getFilesToLint`](getFilesToLint.md)

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
lucode2::autoFormat()
} # }
```
