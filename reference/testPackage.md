# Test package

Installs a package in a temporary library and loads that library on top
of the existing one

## Usage

``` r
testPackage(repo, tmpLib = tempdir(), ...)
```

## Arguments

- repo:

  GitHub repository to install the package from

- tmpLib:

  temporary library directory where the package should be installed

- ...:

  additional arguments forwarded to
  [`devtools::install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
testPackage("git@github.com:pik-piam/lucode2")
} # }
```
